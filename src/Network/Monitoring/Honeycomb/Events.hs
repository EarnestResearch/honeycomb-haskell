module Network.Monitoring.Honeycomb.Events where

import Data.Coerce (coerce)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Monitoring.Honeycomb.Types
import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
import Network.Monitoring.Honeycomb.Types.SendEventsResponse
import Network.URI (URI, normalizeEscape, uriPath)
import RIO

import qualified Data.Aeson as JSON
import qualified Network.HTTP.Client as Client
import qualified RIO.Text as Text

sendEvents
    :: MonadIO m
    => Client.Manager
    -> URI
    -> Dataset
    -> ApiKey
    -> [FrozenHoneyEvent]
    -> m (Client.Response (Maybe SendEventsResponse))
sendEvents manager baseUri dataset apiKey events = do
    initReq <- liftIO . Client.requestFromURI $ baseUri
        { uriPath = uriPath baseUri <> "1/batch/" <> (normalizeEscape . Text.unpack $ coerce dataset) } 
    let req = initReq
         { Client.method = "POST"
         , Client.requestHeaders = additionalRequestHeaders <> Client.requestHeaders initReq
         , Client.requestBody = Client.RequestBodyLBS $ JSON.encode events
         }
    response <- liftIO $ Client.httpLbs req manager
    pure $ JSON.decode' <$> response
  where
    additionalRequestHeaders :: RequestHeaders
    additionalRequestHeaders =
        [ ("Content-Type", "application-json")
        , ("User-Agent", "libhoney-er-haskell/0.1.0.0")
        , ("X-Honeycomb-Team", encodeUtf8 $ coerce apiKey)
        ]
