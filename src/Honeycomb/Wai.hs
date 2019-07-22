{-# LANGUAGE TypeOperators #-}
module Honeycomb.Wai where

import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Honeycomb.Trace
import RIO

import qualified RIO.HashMap as HM
import qualified RIO.List as List
import qualified RIO.Text as Text

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

traceApplicationT
    :: forall m env .
       ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env
       )
    => ServiceName
    -> SpanName
    -> MiddlewareT m
traceApplicationT serviceName spanName =
    traceApplicationT' serviceName spanName readTraceHeader
  where
    readTraceHeader :: Request -> Maybe SpanReference
    readTraceHeader req = do
        let headers = requestHeaders req
        (_, headerValue) <- List.find (\(name, _) -> name == "X-Honeycomb-Trace") headers
        let parts = Text.split (== '=') <$> Text.split (== ';') (decodeUtf8Lenient headerValue)
        (_ : tid) <- List.find (getVal "trace_id") parts
        (_ : sid) <- List.find (getVal "parent_id") parts
        pure $ SpanReference (TraceId $ Text.intercalate "=" tid) (SpanId $ Text.intercalate "=" sid)

    getVal :: Text -> [Text] -> Bool
    getVal header values =
        case values of
            [] -> False
            _ : [] -> False
            h : _ -> h == header

traceApplicationT'
    :: forall m env .
       ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env
       )
    => ServiceName
    -> SpanName
    -> (Request -> Maybe SpanReference)
    -> MiddlewareT m
traceApplicationT' serviceName name parentSpanRef app req inner =
    withNewRootSpan' serviceName name (parentSpanRef req) $ do
        add getRequestFields
        (\x y -> app x y `catchAny` reportErrorStatus) req (\response -> do
            add (getResponseFields response)
            inner response
            )
  where
    getRequestFields :: HoneyObject
    getRequestFields =
        HM.fromList
        [ ("request.http_version", HoneyString . tshow $ httpVersion req)
        , ("request.method", HoneyString . decodeUtf8Lenient $ requestMethod req)
        , ("request.header.user_agent", maybe HoneyNull (HoneyString . decodeUtf8Lenient) (requestHeaderUserAgent req))
        , ("request.host", maybe HoneyNull (HoneyString . decodeUtf8Lenient) (requestHeaderHost req))
        , ("request.path", HoneyString . decodeUtf8Lenient $ rawPathInfo req)
        , ("request.query", HoneyString . decodeUtf8Lenient $ rawQueryString req)
        , ("request.scheme", HoneyString $ if isSecure req then "https" else "http")
        , ("request.secure", HoneyBool $ isSecure req)
        ]

    reportErrorStatus :: SomeException -> m a
    reportErrorStatus e = addField "response.status_code" (500 :: Int) >> throwIO e

    getResponseFields :: Response -> HoneyObject
    getResponseFields response = HM.fromList
        [ ("response.status_code", toHoneyValue . statusCode $ responseStatus response)
        ]
