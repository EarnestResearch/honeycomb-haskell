{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Clients where

import qualified Data.Aeson as JSON
import qualified Honeycomb.Api as Api
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.HTTP.Types as HTTP
import RIO
import qualified RIO.ByteString.Lazy as BL

multipleSuccessfulResponses :: Client.Request -> IO (Client.Response BL.ByteString)
multipleSuccessfulResponses req =
  let numResponses =
        case Client.requestBody req of
          Client.RequestBodyLBS b ->
            maybe 0 length (JSON.decode b :: Maybe [Api.Event])
          _ -> 0
      body = JSON.encode $
        replicate numResponses Api.SendEventsServerReply {Api.serverReplyStatus = 202, Api.serverReplyError = Nothing}
   in pure Client.Response
        { Client.responseStatus = HTTP.status200,
          Client.responseVersion = HTTP.http11,
          Client.responseHeaders = [("Content-Type", "application/json")],
          Client.responseBody = body,
          Client.responseCookieJar = Client.createCookieJar [],
          Client.responseClose' = Client.ResponseClose (pure ())
        }
