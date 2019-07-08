module Network.Monitoring.Honeycomb.Api.Events
    ( sendEvents
    )
where

import Data.Coerce (coerce)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Monitoring.Honeycomb.Api.Types
import Network.URI (normalizeEscape, uriPath)
import RIO

import qualified Data.Aeson as JSON
import qualified Network.HTTP.Client as Client
import qualified RIO.Text as Text

sendEvents
    :: MonadIO m
    => Client.Manager
    -> RequestOptions
    -> [Event]
    -> m (Client.Response (Maybe [Integer]))
sendEvents manager requestOptions events = do
    let baseUri = requestOptions ^. requestApiHostL
    initReq <- liftIO . Client.requestFromURI $ baseUri
        { uriPath = uriPath baseUri <> "1/batch/" <> (normalizeEscape . Text.unpack . coerce $ requestOptions ^. requestApiDatasetL) } 
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
        , ("User-Agent", "libhoney-hs-er/0.1.0.0")
        , ("X-Honeycomb-Team", encodeUtf8 . coerce $ requestOptions ^. requestApiKeyL)
        ]
