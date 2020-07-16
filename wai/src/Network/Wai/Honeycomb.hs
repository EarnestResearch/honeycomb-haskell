{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Honeycomb
  ( spanContextKey,
    traceApplicationT,
  )
where

import Control.Monad.Reader (MonadReader, local)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vault.Lazy as V
import Honeycomb.Trace
import Lens.Micro (over)
import Lens.Micro.Mtl (view)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.UnliftIO (MiddlewareT)
import System.IO.Unsafe
import UnliftIO

decodeUtf8Lenient :: ByteString -> T.Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

traceApplicationT ::
  forall m env.
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  ServiceName ->
  SpanName ->
  MiddlewareT m
traceApplicationT service sp =
  traceApplicationT' service sp readTraceHeader
  where
    readTraceHeader :: Request -> Maybe SpanReference
    readTraceHeader req = do
      let headers = requestHeaders req
      (_, headerValue) <- List.find (\(name, _) -> name == "X-Honeycomb-Trace") headers
      headerText <- getV1 $ decodeUtf8Lenient headerValue
      let parts = T.split (== '=') <$> T.split (== ',') headerText
      (_ : tid) <- List.find (getVal "trace_id") parts
      (_ : sid) <- List.find (getVal "parent_id") parts
      pure $ SpanReference (TraceId $ T.intercalate "=" tid) (SpanId $ T.intercalate "=" sid)
    getVal :: T.Text -> [T.Text] -> Bool
    getVal header values =
      case values of
        [] -> False
        [_] -> False
        h : _ -> h == header
    getV1 :: T.Text -> Maybe T.Text
    getV1 t =
      if T.take 2 t == "1;"
        then Just $ T.drop 2 t
        else Nothing

spanContextKey :: V.Key (IORef (Maybe SpanContext))
spanContextKey = unsafePerformIO V.newKey
{-# NOINLINE spanContextKey #-}

traceApplicationT' ::
  forall m env.
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  ServiceName ->
  SpanName ->
  (Request -> Maybe SpanReference) ->
  MiddlewareT m
traceApplicationT' service sp parentSpanRef app req inner =
  withNewRootSpan' service sp (parentSpanRef req) $ do
    spanContext <- view spanContextL
    spanContextRef <- newIORef spanContext
    add getRequestFields
    (\x y -> app x y `catchAny` reportErrorStatus)
      req {vault = V.insert spanContextKey spanContextRef (vault req)}
      ( \response -> do
          newSpanContext <- readIORef spanContextRef
          local (over spanContextL (const newSpanContext)) $ do
            add (getResponseFields response)
            inner response
      )
  where
    getRequestFields :: HoneyObject
    getRequestFields =
      HoneyObject $
        HM.fromList
          [ ("request.http_version", toHoneyValue . T.pack . show $ httpVersion req),
            ("request.method", toHoneyValue $ requestMethod req),
            ("request.header.user_agent", toHoneyValue $ requestHeaderUserAgent req),
            ("request.host", toHoneyValue $ requestHeaderHost req),
            ("request.path", toHoneyValue $ rawPathInfo req),
            ("request.query", toHoneyValue . (\t -> fromMaybe t $ T.stripPrefix "?" t) . decodeUtf8Lenient $ rawQueryString req),
            ("request.scheme", HoneyString $ if isSecure req then "https" else "http"),
            ("request.secure", toHoneyValue $ isSecure req)
          ]
    reportErrorStatus :: SomeException -> m a
    reportErrorStatus e = addField "response.status_code" (500 :: Int) >> throwIO e
    getResponseFields :: Response -> HoneyObject
    getResponseFields response =
      HoneyObject $
        HM.fromList
          [ ("response.status_code", toHoneyValue . statusCode $ responseStatus response)
          ]
