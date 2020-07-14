{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Honeycomb where

import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Honeycomb.Trace
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import UnliftIO

decodeUtf8Lenient :: ByteString -> T.Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

type MiddlewareT m = ApplicationT m -> ApplicationT m

liftApplication :: MonadUnliftIO m => Application -> ApplicationT m
liftApplication app req respond =
  withRunInIO $ \runInIO -> liftIO $ app req (runInIO . respond)

liftMiddleware :: MonadUnliftIO m => Middleware -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  withRunInIO $ \runInIO -> mid app' req (runInIO . respond)

runApplicationT :: MonadUnliftIO m => ApplicationT m -> m Application
runApplicationT app =
  withRunInIO $ \runInIO ->
    pure $ \req respond ->
      runInIO $ app req (liftIO . respond)

runMiddlewareT :: MonadUnliftIO m => MiddlewareT m -> m Middleware
runMiddlewareT mid =
  withRunInIO $ \runInIO ->
    pure $ \app req respond -> do
      app' <- runInIO . runApplicationT . mid $ liftApplication app
      app' req respond

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
    add getRequestFields
    (\x y -> app x y `catchAny` reportErrorStatus)
      req
      ( \response -> do
          add (getResponseFields response)
          inner response
      )
  where
    getRequestFields :: HoneyObject
    getRequestFields =
      HoneyObject $
        HM.fromList
          [ ("request.http_version", HoneyString . T.pack . show $ httpVersion req),
            ("request.method", HoneyString . decodeUtf8Lenient $ requestMethod req),
            ("request.header.user_agent", maybe HoneyNull (HoneyString . decodeUtf8Lenient) (requestHeaderUserAgent req)),
            ("request.host", maybe HoneyNull (HoneyString . decodeUtf8Lenient) (requestHeaderHost req)),
            ("request.path", HoneyString . decodeUtf8Lenient $ rawPathInfo req),
            ("request.query", HoneyString . (\t -> fromMaybe t $ T.stripPrefix "?" t) . decodeUtf8Lenient $ rawQueryString req),
            ("request.scheme", HoneyString $ if isSecure req then "https" else "http"),
            ("request.secure", HoneyBool $ isSecure req)
          ]
    reportErrorStatus :: SomeException -> m a
    reportErrorStatus e = addField "response.status_code" (500 :: Int) >> throwIO e
    getResponseFields :: Response -> HoneyObject
    getResponseFields response =
      HoneyObject $
        HM.fromList
          [ ("response.status_code", toHoneyValue . statusCode $ responseStatus response)
          ]
