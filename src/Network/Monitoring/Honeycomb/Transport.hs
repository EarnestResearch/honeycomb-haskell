module Network.Monitoring.Honeycomb.Transport
    ( newTransport
    , withTransport
    ) where

import Control.Concurrent.STM.TBQueue (flushTBQueue, lengthTBQueue)
import Data.Coerce (coerce)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.Monitoring.Honeycomb.Events (sendEvents)
import Network.Monitoring.Honeycomb.Types
import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
import Network.URI (URI, uriPath, uriToString)
import RIO
import RIO.Deque
import RIO.Time
import System.IO (print)

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text
import qualified RIO.HashMap as HM
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TL
import qualified RIO.ByteString.Lazy as LBS

type SendKey = (URI, Dataset, ApiKey)

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
    readFromQueueOrWaitSTM :: TVar Bool -> STM (Bool, [FrozenHoneyEvent])
    readFromQueueOrWaitSTM timeout = do
        let q = transportState ^. transportSendQueueL
            flushQ = transportState ^. transportFlushQueueL
        shouldClose <- readTVar closeQ
        flushRequest <- tryReadTMVar flushQ
        currentQueueSize <- lengthTBQueue q
        hasTimedOut <- readTVar timeout
        
        case flushRequest of
            Just flushVar ->
                (False,) <$> (putTMVar flushVar () >> flushTBQueue q)  -- acknowledge all flush requests
            Nothing ->
                if shouldClose
                    || currentQueueSize >= 100
                    || (hasTimedOut && currentQueueSize > 0)
                then
                    (shouldClose,) <$> flushTBQueue q
                else
                    retrySTM

    readFromQueueOrWait :: m (Bool, [FrozenHoneyEvent])
    readFromQueueOrWait = do
        delay <- registerDelay $ 1000 * 50  -- timeout = 50ms
        atomically $ readFromQueueOrWaitSTM delay

sendGroup :: MonadUnliftIO m => Manager -> SendKey -> [FrozenHoneyEvent] -> m ()
sendGroup manager (uri, dataset, apiKey) events = tryAny (void $ sendEvents manager uri dataset apiKey events) >>= fromEither

sendGroups :: MonadUnliftIO m => Manager -> Map SendKey [FrozenHoneyEvent] -> m ()
sendGroups manager groups = sequence_ $ Map.mapWithKey (sendGroup manager) groups

splitEvents :: MonadUnliftIO m => Manager -> [FrozenHoneyEvent] -> m ()
splitEvents manager events =
    sendGroups manager $ Map.fromListWith (++) (fmap toEntry events)
  where
    toEntry :: FrozenHoneyEvent -> (SendKey, [FrozenHoneyEvent])
    toEntry evt =
        ( ( evt ^. frozenEventApiHostL
          , evt ^. frozenEventDatasetL
          , evt ^. frozenEventApiKeyL
          )
        , [evt] 
        )
