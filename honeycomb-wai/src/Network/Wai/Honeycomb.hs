{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Honeycomb
  ( spanContextFromRequest,
    traceApplication,
    traceApplication',
    traceApplicationT,
    traceApplicationT',
    withSpanContextFromRequest,
  )
where

import Control.Monad.Reader (MonadReader, join, local)
import qualified Data.HashMap.Strict as HM
import Data.IP
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vault.Lazy as V
import qualified Honeycomb.Trace as HC
import Lens.Micro (over)
import Lens.Micro.Mtl (view)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.UnliftIO
import System.IO.Unsafe
import UnliftIO
import Data.Bifunctor (Bifunctor(second))

spanContextKey :: V.Key (Maybe HC.SpanContext)
spanContextKey = unsafePerformIO V.newKey
{-# NOINLINE spanContextKey #-}

spanContextFromRequest :: Request -> Maybe HC.SpanContext
spanContextFromRequest = join . V.lookup spanContextKey . vault

withSpanContextFromRequest :: (HC.HasSpanContext env, MonadReader env m) => Request -> m a -> m a
withSpanContextFromRequest req = local (over HC.spanContextL (const $ spanContextFromRequest req))

getRequestFields :: Request -> HC.HoneyObject
getRequestFields req =
  HC.HoneyObject
    . HM.fromList
    $ [ ("request.content_length", HC.toHoneyValue . bodyLength $ requestBodyLength req),
        ("request.header.accept", HC.toHoneyValue $ snd <$> List.find (\(n, _) -> n == "Accept") (requestHeaders req)),
        ("request.header.content_type", HC.toHoneyValue $ snd <$> List.find (\(n, _) -> n == "Content-Type") (requestHeaders req)),
        ("request.header.user_agent", HC.toHoneyValue $ requestHeaderUserAgent req),
        ("request.host", HC.toHoneyValue $ requestHeaderHost req),
        ("request.http_version", HC.toHoneyValue . T.pack . show $ httpVersion req),
        ("request.method", HC.toHoneyValue $ requestMethod req),
        ("request.path", HC.toHoneyValue $ rawPathInfo req),
        ("request.query", HC.toHoneyValue . (\t -> fromMaybe t $ T.stripPrefix "?" t) . decodeUtf8With lenientDecode $ rawQueryString req),
        ("request.remote_addr", HC.toHoneyValue . reqAddr . fromSockAddr $ remoteHost req),
        ("request.scheme", HC.HoneyString $ if isSecure req then "https" else "http"),
        ("request.secure", HC.toHoneyValue $ isSecure req),
        ("type", "http_server")
      ]
      <> fmap (\(name, value) -> ("http.query_param." <> decodeUtf8With lenientDecode name, HC.toHoneyValue value)) (queryString req)
  where
    bodyLength = \case
      ChunkedBody -> Nothing
      KnownLength len | len > 0 -> Just $ toInteger len
      KnownLength _ -> Nothing
    reqAddr = \case
      Just (ip, _) -> Just $ show ip
      Nothing -> Nothing

reportErrorStatus :: (MonadIO m, MonadReader env m, HC.HasSpanContext env) => SomeException -> m a
reportErrorStatus e = HC.addField "response.status_code" (500 :: Int) >> throwIO e

getResponseFields :: Response -> HC.HoneyObject
getResponseFields response =
  HC.HoneyObject $
    HM.fromList
      [ ("response.status_code", HC.toHoneyValue . statusCode $ responseStatus response)
      ]

traceApplication ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  m Middleware
traceApplication service sp =
  traceApplication' service sp (const Nothing)

traceApplication' ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  (Request -> Maybe HC.HoneyObject) ->
  m Middleware
traceApplication' service sp extraReqDetails =
  runMiddlewareT $ traceApplicationT' service sp extraReqDetails

traceApplicationT ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  MiddlewareT m
traceApplicationT service sp =
  traceApplicationT' service sp (const Nothing)

traceApplicationT' ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  (Request -> Maybe HC.HoneyObject) ->
  MiddlewareT m
traceApplicationT' service sp extraReqDetails app req inner =
  HC.withNewRootSpan' service sp (getFirstTraceHeader traceFormats req) $ do
    spanContext <- view HC.spanContextL
    HC.add (getRequestFields req)
    (\x y -> app x y `catchAny` reportErrorStatus)
      req {vault = V.insert spanContextKey spanContext (vault req)}
      ( \response -> do
          HC.add (getResponseFields response)
          maybe (pure ()) HC.add (extraReqDetails req)
          inner response
      )
  where
    traceFormats = [HC.honeycombTraceHeaderFormat, HC.w3cTraceHeaderFormat]

getFirstTraceHeader :: [HC.TraceHeaderFormat] -> Request -> Maybe HC.SpanReference
getFirstTraceHeader traceFormats req =
  join $ List.find isJust (parseWithFormat <$> traceFormats)
  where
    parseWithFormat :: HC.TraceHeaderFormat -> Maybe HC.SpanReference
    parseWithFormat f = HC.parseTraceHeader f $ second (decodeUtf8With lenientDecode) <$> requestHeaders req
