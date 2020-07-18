{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Honeycomb
  ( spanContextKey,
    traceApplicationT,
    readTraceHeader,
    decodeUtf8Lenient,
    getRequestFields,
    reportErrorStatus,
    getResponseFields,
  )
where

import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vault.Lazy as V
import qualified Honeycomb.Trace as HC
import Lens.Micro.Mtl (view)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.UnliftIO (MiddlewareT)
import System.IO.Unsafe
import UnliftIO

decodeUtf8Lenient :: ByteString -> T.Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

readTraceHeader :: Request -> Maybe HC.SpanReference
readTraceHeader req = do
  let headers = requestHeaders req
  (_, headerValue) <- List.find (\(name, _) -> name == "X-Honeycomb-Trace") headers
  headerText <- getV1 $ decodeUtf8Lenient headerValue
  let parts = T.split (== '=') <$> T.split (== ',') headerText
  (_ : tid) <- List.find (getVal "trace_id") parts
  (_ : sid) <- List.find (getVal "parent_id") parts
  pure $ HC.SpanReference (HC.TraceId $ T.intercalate "=" tid) (HC.SpanId $ T.intercalate "=" sid)

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

traceApplicationT ::
  forall m env.
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  MiddlewareT m
traceApplicationT service sp =
  traceApplicationT' service sp readTraceHeader

spanContextKey :: V.Key (Maybe HC.SpanContext)
spanContextKey = unsafePerformIO V.newKey
{-# NOINLINE spanContextKey #-}

getRequestFields :: Request -> HC.HoneyObject
getRequestFields req =
  HC.HoneyObject $
    HM.fromList
      [ ("request.http_version", HC.toHoneyValue . T.pack . show $ httpVersion req),
        ("request.method", HC.toHoneyValue $ requestMethod req),
        ("request.header.user_agent", HC.toHoneyValue $ requestHeaderUserAgent req),
        ("request.host", HC.toHoneyValue $ requestHeaderHost req),
        ("request.path", HC.toHoneyValue $ rawPathInfo req),
        ("request.query", HC.toHoneyValue . (\t -> fromMaybe t $ T.stripPrefix "?" t) . decodeUtf8Lenient $ rawQueryString req),
        ("request.scheme", HC.HoneyString $ if isSecure req then "https" else "http"),
        ("request.secure", HC.toHoneyValue $ isSecure req)
      ]

reportErrorStatus :: (MonadIO m, MonadReader env m, HC.HasSpanContext env) => SomeException -> m a
reportErrorStatus e = HC.addField "response.status_code" (500 :: Int) >> throwIO e

getResponseFields :: Response -> HC.HoneyObject
getResponseFields response =
  HC.HoneyObject $
    HM.fromList
      [ ("response.status_code", HC.toHoneyValue . statusCode $ responseStatus response)
      ]

traceApplicationT' ::
  forall m env.
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  (Request -> Maybe HC.SpanReference) ->
  MiddlewareT m
traceApplicationT' service sp parentSpanRef app req inner =
  HC.withNewRootSpan' service sp (parentSpanRef req) $ do
    spanContext <- view HC.spanContextL
    HC.add (getRequestFields req)
    (\x y -> app x y `catchAny` reportErrorStatus)
      req {vault = V.insert spanContextKey spanContext (vault req)}
      ( \response -> do
          HC.add (getResponseFields response)
          inner response
      )
