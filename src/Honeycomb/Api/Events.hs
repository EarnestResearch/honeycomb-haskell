{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Api.Events
  ( sendEvents,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Int (Int64)
import Honeycomb.Api.Types
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Header (RequestHeaders)
import Network.URI (normalizeEscape)
import UnliftIO

maxEntryLength :: Int64
maxEntryLength = 100000

maxBodyLength :: Int64
maxBodyLength = 5000000

-- | Send a batch of events to Honeycomb servers.
--
-- Given an implementation of the HTTP call made (which for production
-- service, should be the implementation from "Network.HTTP.Client"), this
-- will attempt to send a set of events to Honeycomb's servers. If only
-- some of the events were sent, it will loop around resending until all
-- events have completed sending (or are dropped due to size issues).
sendEvents ::
  MonadUnliftIO m =>
  (Client.Request -> m (Client.Response LBS.ByteString)) ->
  RequestOptions ->
  [Event] ->
  m SendEventsResponse
sendEvents httpLbs requestOptions events = do
  req <- liftIO $ newReq <$> Client.parseRequest batchUri
  let (unsent, dropped, body) = createBodyFromEvents True events [] "[" 1
      reqWithBody = req body
  responseOrError <- try $ httpLbs reqWithBody
  response <- case responseOrError of
    Left (Client.HttpExceptionRequest _ Client.ConnectionTimeout) -> httpLbs reqWithBody
    Left (Client.HttpExceptionRequest _ Client.ResponseTimeout) -> httpLbs reqWithBody
    Left e -> throwIO e
    Right result -> pure result
  pure SendEventsResponse
    { unsentEvents = unsent,
      oversizedEvents = dropped,
      serviceResponse = JSON.decode <$> response
    }
  where
    createBodyFromEvents ::
      Bool ->
      [Event] ->
      [Event] ->
      LBS.ByteString ->
      Int64 ->
      ([Event], [Event], LBS.ByteString)
    createBodyFromEvents isFirst toSend dropped bs bsLength =
      case toSend of
        [] -> ([], dropped, bs <> "]")
        l@(hd : tl) ->
          let newEncoded = JSON.encode hd
              newEncodedLength = LBS.length newEncoded
              newBS =
                if isFirst
                  then bs <> JSON.encode hd
                  else bs <> "," <> JSON.encode hd
              newBSLength =
                if isFirst
                  then bsLength + newEncodedLength
                  else bsLength + 1 + newEncodedLength
           in if newEncodedLength > maxEntryLength
                then createBodyFromEvents isFirst tl (hd : dropped) bs bsLength
                else
                  if newBSLength + 1 >= maxBodyLength
                    then (l, dropped, bs <> "]")
                    else createBodyFromEvents False tl dropped newBS newBSLength
    newReq :: Client.Request -> LBS.ByteString -> Client.Request
    newReq initReq body =
      initReq
        { Client.method = "POST",
          Client.requestHeaders = additionalRequestHeaders <> Client.requestHeaders initReq,
          Client.requestBody = Client.RequestBodyLBS body
        }
    batchUri = (T.unpack . coerce $ requestOptions ^. requestApiHostL) <> "1/batch/" <> batchPath
    batchPath = normalizeEscape . T.unpack . coerce $ requestOptions ^. requestApiDatasetL
    additionalRequestHeaders :: RequestHeaders
    additionalRequestHeaders =
      [ ("Content-Type", "application-json"),
        ("User-Agent", "libhoney-hs-er/0.1.0.0"),
        ("X-Honeycomb-Team", TE.encodeUtf8 . coerce $ requestOptions ^. requestApiKeyL)
      ]
