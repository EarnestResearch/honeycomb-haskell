module Network.Monitoring.Honeycomb.Transport
    ( newTransport
    , withTransport
    ) where

import Data.Coerce (coerce)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.Monitoring.Honeycomb.Types
import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
import Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
import Network.URI (URI, uriPath)
import RIO
import RIO.Time
import System.IO (print)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text
import qualified RIO.HashMap as HM
import qualified RIO.Text as Text
import qualified RIO.Text.Lazy as TL
import qualified RIO.ByteString.Lazy as LBS

newTransport
    :: forall n m .
       ( MonadUnliftIO n
       , MonadIO m
       )
    => HoneyServerOptions
    -> n (TBQueue HoneyQueueMessage, m ())
newTransport honeyServerOptions = do
    sendQueue <- atomically $ newTBQueue $ honeyServerOptions ^. pendingWorkCapacityL
    httpManager <- liftIO $ newManager tlsManagerSettings
    readThreadId <- async $ readQueue httpManager sendQueue
    let shutdown = atomically (writeTBQueue sendQueue CloseQueue) >> wait readThreadId
    pure (sendQueue, shutdown)

withTransport
    :: MonadUnliftIO m
    => HoneyServerOptions
    -> (TBQueue HoneyQueueMessage -> m a)
    -> m a
withTransport honeyServerOptions inner = withRunInIO $ \run ->
    bracket (newTransport honeyServerOptions)
        snd
        (run . inner . fst)
  
requestFromEvent :: MonadIO m => FrozenHoneyEvent -> m Request
requestFromEvent evt = do
    initialReq <- liftIO $ createRequest (evt ^. frozenEventApiHostL) (evt ^. frozenEventDatasetL)
    let evtTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (evt ^. frozenEventTimestampL)
    pure initialReq {
          method = "POST"
        , requestHeaders =
            [ ("Content-Type",           "application/json")
            , ("User-Agent",             "libhoney-er-haskell/0.1.0.0")
            , ("X-Honeycomb-Team",       encodeUtf8 . coerce $ evt ^. frozenEventApiKeyL)
            , ("X-Honeycomb-Event-Time", encodeUtf8 $ Text.pack evtTime)
            , ("X-Honeycomb-Samplerate", encodeUtf8 . tshow $ evt ^. frozenEventSampleRateL)
            ] <> requestHeaders initialReq
        , requestBody = RequestBodyLBS $ Aeson.encode $ evt ^. frozenEventFieldsL
        }

sendEventFromQueue
    :: MonadIO m
    => Manager
    -> FrozenHoneyEvent
    -> m (Response LBS.ByteString, NominalDiffTime)
sendEventFromQueue manager evt = do
    req <- requestFromEvent evt
    start <- getCurrentTime
    response <- liftIO $ httpLbs req manager
    end <- getCurrentTime
    let duration = diffUTCTime end start
    pure (response, duration)
    
createRequest :: URI -> Dataset -> IO Request
createRequest baseUri (Dataset ds) =
    requestFromURI $ baseUri { uriPath = uriPath baseUri <> "1/events/" <> Text.unpack ds }

sendEvt :: MonadIO m => Manager -> FrozenHoneyEvent -> m ()
sendEvt httpManager evt = do
    (response, latency) <- sendEventFromQueue httpManager evt
    let status = responseStatus response
        obj = (Aeson.decode $ responseBody response) :: Maybe Aeson.Object
        honeyResponse = HoneyResponse (statusCode status) latency $ TL.toStrict . Data.Aeson.Text.encodeToLazyText <$> (HM.lookup "error" =<< obj)
    liftIO $ print honeyResponse

readQueue :: MonadUnliftIO m => Manager -> TBQueue HoneyQueueMessage -> m ()
readQueue httpManager q = do
    curVal <- atomically $ readTBQueue q
    case curVal of
        QueueMessage evt -> tryAny (sendEvt httpManager evt) >> readQueue httpManager q
        CloseQueue       -> pure ()
        FlushQueue mvar  -> putMVar mvar () >> readQueue httpManager q
