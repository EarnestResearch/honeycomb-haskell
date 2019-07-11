module Honeycomb.Api.Events
    ( sendEvents
    )
where

import Data.Coerce (coerce)
import Network.HTTP.Types.Header (RequestHeaders)
import Honeycomb.Api.Types
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
    initReq <- liftIO $ Client.requestFromURI batchUri
    response <- liftIO $ Client.httpLbs (newReq initReq) manager
    pure $ JSON.decode' <$> response
  where
    newReq :: Client.Request -> Client.Request
    newReq initReq = initReq
        { Client.method = "POST"
        , Client.requestHeaders = additionalRequestHeaders <> Client.requestHeaders initReq
        , Client.requestBody = Client.RequestBodyLBS $ JSON.encode events
        }

    batchUri =
        let baseUri = requestOptions ^. requestApiHostL in
          baseUri { uriPath = uriPath baseUri <> "1/batch/" <> batchPath }

    batchPath = normalizeEscape . Text.unpack . coerce $ requestOptions ^. requestApiDatasetL

    additionalRequestHeaders :: RequestHeaders
    additionalRequestHeaders =
        [ ("Content-Type", "application-json")
        , ("User-Agent", "libhoney-hs-er/0.1.0.0")
        , ("X-Honeycomb-Team", encodeUtf8 . coerce $ requestOptions ^. requestApiKeyL)
        ]
