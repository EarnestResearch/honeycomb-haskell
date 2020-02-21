{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Honeycomb.Transport
  ( newTransport,
    withTransport,
  )
where

import Control.Concurrent.STM.TBQueue (flushTBQueue, lengthTBQueue)
import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import Honeycomb.Api.Events (sendEvents)
import qualified Honeycomb.Api.Types as Api
import Honeycomb.Core.Internal.Types
import Honeycomb.Core.Types
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP
import UnliftIO

newTransport ::
  forall n m.
  ( MonadUnliftIO n,
    MonadIO m
  ) =>
  HoneyServerOptions n ->
  n (TransportState, m ())
newTransport honeyServerOptions = do
  transportState <-
    mkTransportState
      (honeyServerOptions ^. pendingWorkCapacityL)
      (honeyServerOptions ^. pendingWorkCapacityL) -- [todo] use different parameters
  closeQueue <- newTVarIO False
  readThreadId <- async $ readQueue honeyServerOptions transportState closeQueue
  link readThreadId -- [todo] think about whether this is a good idea
  let shutdown = atomically (writeTVar closeQueue True) >> wait readThreadId
  pure (transportState, shutdown)

withTransport ::
  MonadUnliftIO m =>
  HoneyServerOptions m ->
  (TransportState -> m a) ->
  m a
withTransport honeyServerOptions inner =
  bracket
    (newTransport honeyServerOptions)
    snd
    (inner . fst)

writeToResponseQueue :: MonadIO m => HoneyServerOptions m -> TransportState -> HoneyResponse -> m ()
writeToResponseQueue honeyServerOptions transportState response =
  let shouldBlockOnResponses = honeyServerOptions ^. blockOnResponseL
      responseQueue = transportState ^. transportResponseQueueL
   in atomically $
        if shouldBlockOnResponses
          then writeTBQueue responseQueue response
          else do
            isFull <- isFullTBQueue responseQueue
            if isFull
              then pure () -- dropping response status
              else writeTBQueue responseQueue response

readQueue ::
  forall m.
  MonadUnliftIO m =>
  HoneyServerOptions m ->
  TransportState ->
  TVar Bool ->
  m ()
readQueue honeyServerOptions transportState closeQ = do
  (shouldStop, events) <- readFromQueueOrWait
  if shouldStop
    then sendEventsAndRespond honeyServerOptions transportState events
    else sendEventsAndRespond honeyServerOptions transportState events >> readQueue honeyServerOptions transportState closeQ
  where
    readFromQueueOrWaitSTM :: TVar Bool -> STM (Bool, [(Api.RequestOptions, Api.Event)])
    readFromQueueOrWaitSTM timeoutAfter = do
      let q = transportState ^. transportSendQueueL
          flushQ = transportState ^. transportFlushQueueL
      shouldClose <- readTVar closeQ
      flushRequests <- flushTBQueue flushQ
      currentQueueSize <- lengthTBQueue q
      hasTimedOut <- readTVar timeoutAfter
      checkSTM $
        shouldClose
          || not (List.null flushRequests)
          || currentQueueSize >= honeyServerOptions ^. maxBatchSizeL
          || (hasTimedOut && currentQueueSize > 0)
      traverse_ (`putTMVar` ()) flushRequests
      (shouldClose,) <$> flushTBQueue q
    readFromQueueOrWait :: m (Bool, [(RequestOptions, Api.Event)])
    readFromQueueOrWait = do
      delay <- registerDelay $ (honeyServerOptions ^. sendFrequencyL) * 1000
      atomically $ readFromQueueOrWaitSTM delay

-- | Sends a set of events, which have been grouped by `RequestOptions`.
--
-- For simplicity, if this fails to send all messages due to max size limits,
-- it will keep trying until messages are sent
sendEventGroupAndRespond ::
  forall m.
  MonadUnliftIO m =>
  HoneyServerOptions m ->
  TransportState ->
  RequestOptions ->
  [Api.Event] ->
  m ()
sendEventGroupAndRespond honeyServerOptions transportState requestOptions reversedEvents =
  if List.null reversedEvents
    then pure ()
    else sendEventGroupAndRespond' (List.reverse reversedEvents)
  where
    sendEventGroupAndRespond' :: [Api.Event] -> m ()
    sendEventGroupAndRespond' events = do
      sendResult <- tryAny $ sendEvents (honeyServerOptions ^. httpLbsL) requestOptions events
      sendResponses $ getResponsesForEvents events sendResult
      sendUnsentEvents $ Api.unsentEvents <$> sendResult
    getTransmitErrorForEvent :: SomeException -> Api.Event -> HoneyResponse
    getTransmitErrorForEvent exc event = HoneyResponse
      { honeyResponseMetadata = event ^. Api.eventMetadataL,
        honeyResponseStatusCode = Nothing,
        honeyResponseError = Just . T.pack $ show exc
      }
    getOversizedErrorForEvent :: Api.Event -> HoneyResponse
    getOversizedErrorForEvent event = HoneyResponse
      { honeyResponseMetadata = event ^. Api.eventMetadataL,
        honeyResponseStatusCode = Nothing,
        honeyResponseError = Just "Oversized event sent to API"
      }
    getResponseDataForEvent :: Int -> [Api.Event] -> Client.Response (Maybe [Api.SendEventsServerReply]) -> [HoneyResponse]
    getResponseDataForEvent numSentEvents events response =
      case Client.responseBody response of
        Nothing -> getInvalidBodyForEvents
        Just eventReplies ->
          if numSentEvents /= List.length eventReplies
            then getInvalidResultLength
            else getSingleErrorMessage <$> events `zip` eventReplies
      where
        getSingleErrorMessage :: (Api.Event, Api.SendEventsServerReply) -> HoneyResponse
        getSingleErrorMessage (meta, reply) = HoneyResponse
          { honeyResponseMetadata = meta ^. Api.eventMetadataL,
            honeyResponseStatusCode = Just $ Api.serverReplyStatus reply,
            honeyResponseError = Api.serverReplyError reply
          }
        getInvalidBodyForEvents :: [HoneyResponse]
        getInvalidBodyForEvents =
          ( \meta -> HoneyResponse
              { honeyResponseMetadata = meta ^. Api.eventMetadataL,
                honeyResponseStatusCode = Just $ HTTP.statusCode (Client.responseStatus response),
                honeyResponseError = Just "Events batch endpoint returned invalid body"
              }
          )
            <$> events
        getInvalidResultLength :: [HoneyResponse]
        getInvalidResultLength =
          ( \meta -> HoneyResponse
              { honeyResponseMetadata = meta ^. Api.eventMetadataL,
                honeyResponseStatusCode = Nothing,
                honeyResponseError = Just "Events batch endpoint returned incorrect length array"
              }
          )
            <$> events
    sendResponses :: [HoneyResponse] -> m ()
    sendResponses responses = void $ writeToResponseQueue honeyServerOptions transportState `traverse` responses
    getResponsesForEvents :: [Api.Event] -> Either SomeException Api.SendEventsResponse -> [HoneyResponse]
    getResponsesForEvents events (Left err) =
      getTransmitErrorForEvent err <$> events
    getResponsesForEvents events (Right Api.SendEventsResponse {Api.oversizedEvents, Api.serviceResponse}) =
      (getOversizedErrorForEvent <$> oversizedEvents)
        <> getResponseDataForEvent (List.length events - List.length oversizedEvents) events serviceResponse
    sendUnsentEvents :: Either SomeException [Api.Event] -> m ()
    sendUnsentEvents (Right unsent) | List.null unsent = pure ()
    sendUnsentEvents (Right unsent) = sendEventGroupAndRespond' unsent
    sendUnsentEvents (Left _) = pure ()

-- | Send all events and return event status to status channel
--
-- This takes all events, and attempts to send them. For every event, a status
-- message will be sent back to the status channel.
sendEventsAndRespond ::
  forall m.
  MonadUnliftIO m =>
  HoneyServerOptions m ->
  TransportState ->
  -- | The list of events to be sent
  [(RequestOptions, Api.Event)] ->
  m ()
sendEventsAndRespond honeyServerOptions transportState events =
  sendEventGroupsAndRespond $ HM.fromListWith (flip (++)) (fmap toEntry events)
  where
    sendEventGroupsAndRespond ::
      HM.HashMap RequestOptions [Api.Event] ->
      m ()
    sendEventGroupsAndRespond groups =
      sequence_ $ HM.mapWithKey (sendEventGroupAndRespond honeyServerOptions transportState) groups
    toEntry :: (RequestOptions, Api.Event) -> (RequestOptions, [Api.Event])
    toEntry evt = (fst evt, [snd evt])
