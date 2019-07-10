module Network.Monitoring.Honeycomb.Transport
    ( newTransport
    , withTransport
    ) where

import Control.Concurrent.STM.TBQueue (flushTBQueue, lengthTBQueue)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Monitoring.Honeycomb.Api.Events (sendEvents)
import Network.Monitoring.Honeycomb.Api.Types
import Network.Monitoring.Honeycomb.Types
import RIO

import qualified RIO.Map as Map

newTransport
    :: forall n m .
       ( MonadUnliftIO n
       , MonadIO m
       )
    => HoneyServerOptions
    -> n (TransportState, m ())
newTransport honeyServerOptions = do
    transportState <- mkTransportState $ honeyServerOptions ^. pendingWorkCapacityL
    closeQueue <- newTVarIO False
    httpManager <- liftIO $ newManager tlsManagerSettings
    readThreadId <- async $ readQueue httpManager transportState closeQueue
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
    

readQueue :: forall m . MonadUnliftIO m => Manager -> TransportState -> TVar Bool -> m ()
readQueue manager transportState closeQ = do
    (shouldStop, events) <- readFromQueueOrWait
    if shouldStop then
        splitEvents manager events
    else
        splitEvents manager events >> readQueue manager transportState closeQ
  where
    readFromQueueOrWaitSTM :: TVar Bool -> STM (Bool, [(RequestOptions, Event)])
    readFromQueueOrWaitSTM timeoutAfter = do
        let q = transportState ^. transportSendQueueL
            flushQ = transportState ^. transportFlushQueueL
        shouldClose <- readTVar closeQ
        flushRequest <- tryReadTMVar flushQ
        currentQueueSize <- lengthTBQueue q
        hasTimedOut <- readTVar timeoutAfter
        
        case flushRequest of
            Just flushVar ->
                (False,) <$> (putTMVar flushVar () >> flushTBQueue q)  -- acknowledge all flush requests
            Nothing -> do
                checkSTM $
                    shouldClose
                    || currentQueueSize >= 100
                    || (hasTimedOut && currentQueueSize > 0)
                (shouldClose,) <$> flushTBQueue q

    readFromQueueOrWait :: m (Bool, [(RequestOptions, Event)])
    readFromQueueOrWait = do
        delay <- registerDelay $ 1000 * 50  -- timeout = 50ms
        atomically $ readFromQueueOrWaitSTM delay

sendGroup :: MonadUnliftIO m => Manager -> RequestOptions -> [Event] -> m ()
sendGroup manager requestOptions events = void $ tryAny (void $ sendEvents manager requestOptions events)

sendGroups :: MonadUnliftIO m => Manager -> Map RequestOptions [Event] -> m ()
sendGroups manager groups = sequence_ $ Map.mapWithKey (sendGroup manager) groups

splitEvents :: MonadUnliftIO m => Manager -> [(RequestOptions, Event)] -> m ()
splitEvents manager events =
    sendGroups manager $ Map.fromListWith (++) (fmap toEntry events)
  where
    toEntry :: (RequestOptions, Event) -> (RequestOptions, [Event])
    toEntry evt =
        ( fst evt
        , [snd evt]
        )
