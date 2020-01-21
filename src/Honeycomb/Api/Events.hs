{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Api.Events
  ( sendEvents,
  )
where

import Control.Monad.Reader (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Honeycomb.Api.Types
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Header (RequestHeaders)
import Network.URI (normalizeEscape)

sendEvents ::
  MonadIO m =>
  Client.Manager ->
  RequestOptions ->
  [Event] ->
  m (Client.Response (Maybe [Integer]))
sendEvents manager requestOptions events = do
  initReq <- liftIO $ Client.parseRequest batchUri
  response <- liftIO $ Client.httpLbs (newReq initReq) manager
  pure $ JSON.decode' <$> response
  where
    newReq :: Client.Request -> Client.Request
    newReq initReq =
      initReq
        { Client.method = "POST",
          Client.requestHeaders = additionalRequestHeaders <> Client.requestHeaders initReq,
          Client.requestBody = Client.RequestBodyLBS $ JSON.encode events
        }
    batchUri = (T.unpack . coerce $ requestOptions ^. requestApiHostL) <> "1/batch/" <> batchPath
    batchPath = normalizeEscape . T.unpack . coerce $ requestOptions ^. requestApiDatasetL
    additionalRequestHeaders :: RequestHeaders
    additionalRequestHeaders =
      [ ("Content-Type", "application-json"),
        ("User-Agent", "libhoney-hs-er/0.1.0.0"),
        ("X-Honeycomb-Team", TE.encodeUtf8 . coerce $ requestOptions ^. requestApiKeyL)
      ]
