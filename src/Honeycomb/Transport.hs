{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Honeycomb.Transport
  ( newTransport,
    withTransport,
  )
where

import Control.Concurrent.STM.TBQueue (flushTBQueue, lengthTBQueue)
import Control.Monad.Reader (MonadIO)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Honeycomb.Api.Events (sendEvents)
import Honeycomb.Api.Types
import Honeycomb.Core.Types
import Lens.Micro ((^.))
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

readQueue :: forall m. MonadUnliftIO m => HoneyServerOptions m -> TransportState -> TVar Bool -> m ()
readQueue honeyServerOptions transportState closeQ = do
  (shouldStop, events) <- readFromQueueOrWait
  if shouldStop
    then sendEventsAndRespond honeyServerOptions events
    else sendEventsAndRespond honeyServerOptions events >> readQueue honeyServerOptions transportState closeQ
  where
    readFromQueueOrWaitSTM :: TVar Bool -> STM (Bool, [(RequestOptions, Event)])
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
    readFromQueueOrWait :: m (Bool, [(RequestOptions, Event)])
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
  RequestOptions ->
  [Event] ->
  m ()
sendEventGroupAndRespond honeyServerOptions requestOptions reversedEvents =
  if List.null reversedEvents
    then pure ()
    else sendEventGroupAndRespond' (List.reverse reversedEvents)
  where
    sendEventGroupAndRespond' :: [Event] -> m ()
    sendEventGroupAndRespond' events = do
      sendResult <- tryAny $ sendEvents (honeyServerOptions ^. httpLbsL) requestOptions events
      respondToEvents events sendResult
      sendUnsentEvents $ unsentEvents <$> sendResult
    respondToEvents :: [Event] -> Either SomeException SendEventsResponse -> m ()
    respondToEvents _ _ = pure ()
    sendUnsentEvents :: Either SomeException [Event] -> m ()
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
  -- | The list of events to be sent
  [(RequestOptions, Event)] ->
  m ()
sendEventsAndRespond honeyServerOptions events =
  sendEventGroupsAndRespond $ HM.fromListWith (flip (++)) (fmap toEntry events)
  where
    sendEventGroupsAndRespond ::
      HM.HashMap RequestOptions [Event] ->
      m ()
    sendEventGroupsAndRespond groups =
      sequence_ $ HM.mapWithKey (sendEventGroupAndRespond honeyServerOptions) groups
    toEntry :: (RequestOptions, Event) -> (RequestOptions, [Event])
    toEntry evt = (fst evt, [snd evt])
