{-# LANGUAGE TypeOperators #-}
module Network.Monitoring.Honeycomb.Wai where

import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Monitoring.Honeycomb
import Network.Monitoring.Honeycomb.Trace
import RIO

import qualified RIO.HashMap as HM

type Application' m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

liftApplication :: MonadUnliftIO m => Application -> Application' m
liftApplication app req respond =
    withRunInIO $ \runInIO -> liftIO $ app req (runInIO . respond)

unliftApplication :: MonadUnliftIO m => Application' m -> m Application
unliftApplication app =
    withRunInIO $ \runInIO ->
        pure $ \req respond ->
            runInIO $ app req (liftIO . respond)

traceApplication'
    :: forall m env .
       ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasTracer env
       )
    => SpanName
    -> Application' m
    -> Application' m
traceApplication' name app req inner =
    withNewRootSpan name (const mempty) $ do
        addToSpan getRequestFields
        app req (\response -> do
            addToSpan (getResponseFields response)
            inner response
            )
  where
    getRequestFields :: HoneyObject
    getRequestFields = HM.fromList
        [ ("meta.span_type", HoneyString "http_request")
        , ("request.header.user_agent", maybe HoneyNil (toHoneyValue . decodeUtf8Lenient) (requestHeaderUserAgent req))
        , ("request.host", maybe HoneyNil (toHoneyValue . decodeUtf8Lenient) (requestHeaderHost req))
        , ("request.path", toHoneyValue . decodeUtf8Lenient $ rawPathInfo req)
        ]

    getResponseFields :: Response -> HoneyObject
    getResponseFields response = HM.fromList
        [ ("response.status_code", toHoneyValue . statusCode $ responseStatus response)
        ]
