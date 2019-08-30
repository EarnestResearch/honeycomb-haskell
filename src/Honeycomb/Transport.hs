{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Honeycomb.Transport
    ( newTransport
    , withTransport
    ) where

import Control.Concurrent.STM.TBQueue (flushTBQueue, lengthTBQueue)
import Control.Monad.Reader (MonadIO)
import Data.Foldable (traverse_)
import Lens.Micro ((^.))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Honeycomb.Api.Events (sendEvents)
import Honeycomb.Api.Types
import Honeycomb.Types
import UnliftIO

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List

newTransport
    :: forall n m .
       ( MonadUnliftIO n
       , MonadIO m
       )
    => HoneyServerOptions
    -> n (TransportState, m ())
newTransport honeyServerOptions = do
    transportState <- mkTransportState
        (honeyServerOptions ^. pendingWorkCapacityL) (honeyServerOptions ^. pendingWorkCapacityL) -- [todo] use different parameters
    closeQueue <- newTVarIO False
    httpManager <- liftIO $ newManager tlsManagerSettings
    readThreadId <- async $ readQueue honeyServerOptions httpManager transportState closeQueue
    link readThreadId  -- [todo] think about whether this is a good idea
    let shutdown = atomically (writeTVar closeQueue True) >> wait readThreadId
    pure (transportState, shutdown)

withTransport
    :: MonadUnliftIO m
    => HoneyServerOptions
    -> (TransportState -> m a)
    -> m a
withTransport honeyServerOptions inner = withRunInIO $ \run ->
    bracket (newTransport honeyServerOptions)
        snd
        (run . inner . fst)
    

readQueue :: forall m . MonadUnliftIO m => HoneyServerOptions -> Manager -> TransportState -> TVar Bool -> m ()
readQueue honeyServerOptions manager transportState closeQ = do
    (shouldStop, events) <- readFromQueueOrWait
    if shouldStop then
        splitEvents manager events
    else
        splitEvents manager events >> readQueue honeyServerOptions manager transportState closeQ
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

sendGroup :: forall m . MonadUnliftIO m => Manager -> RequestOptions -> [Event] -> m ()
sendGroup manager requestOptions events = do
    sendResult <- tryAny $ sendEvents manager requestOptions events
    respond sendResult
  where
    -- [todo] dummy code
    respond :: Either SomeException (Response (Maybe [Integer])) -> m ()
    respond (Left _) = pure ()
    respond (Right _) = pure ()

sendGroups :: MonadUnliftIO m => Manager -> HM.HashMap RequestOptions [Event] -> m ()
sendGroups manager groups = sequence_ $ HM.mapWithKey (sendGroup manager) groups

splitEvents :: MonadUnliftIO m => Manager -> [(RequestOptions, Event)] -> m ()
splitEvents manager events =
    sendGroups manager $ HM.fromListWith (++) (fmap toEntry events)
  where
    toEntry :: (RequestOptions, Event) -> (RequestOptions, [Event])
    toEntry evt =
        ( fst evt
        , [snd evt]
        )
