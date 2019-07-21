{-# LANGUAGE TypeOperators #-}
module Honeycomb.Wai where

import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Honeycomb.Trace
import RIO

import qualified RIO.HashMap as HM
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
    -> (Request -> Maybe SpanReference)
    -> MiddlewareT m
traceApplicationT serviceName name parentSpanRef app req inner =
    withNewRootSpan' serviceName name (parentSpanRef req) $ do
        add getRequestFields
        (\x y -> app x y `catchAny` reportErrorStatus) req (\response -> do
            add (getResponseFields response)
            inner response
            )
  where
    getRequestFields :: HoneyObject
    getRequestFields =
        let headers = requestHeaders req in
        HM.fromList
        [ ("request.remote_addr", HoneyString . tshow $ remoteHost req)
        , ("request.http_version", HoneyString . tshow $ httpVersion req)
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
