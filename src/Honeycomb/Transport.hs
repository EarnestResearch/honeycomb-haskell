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
import qualified Network.HTTP.Client as Client
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
    then splitEvents honeyServerOptions events
    else splitEvents honeyServerOptions events >> readQueue honeyServerOptions transportState closeQ
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

sendGroup :: forall m. MonadUnliftIO m => HoneyServerOptions m -> RequestOptions -> [Event] -> m ()
sendGroup honeyServerOptions requestOptions events = do
  sendResult <- tryAny $ sendEvents (honeyServerOptions ^. httpLbsL) requestOptions events
  respond sendResult
  where
    -- [todo] dummy code
    respond :: Either SomeException (Client.Response (Maybe [Integer])) -> m ()
    respond (Left _) = pure ()
    respond (Right _) = pure ()

sendGroups :: MonadUnliftIO m => HoneyServerOptions m -> HM.HashMap RequestOptions [Event] -> m ()
sendGroups honeyServerOptions groups = sequence_ $ HM.mapWithKey (sendGroup honeyServerOptions) groups

splitEvents :: MonadUnliftIO m => HoneyServerOptions m -> [(RequestOptions, Event)] -> m ()
splitEvents honeyServerOptions events =
  sendGroups honeyServerOptions $ HM.fromListWith (++) (fmap toEntry events)
  where
    toEntry :: (RequestOptions, Event) -> (RequestOptions, [Event])
    toEntry evt =
      ( fst evt,
        [snd evt]
      )
