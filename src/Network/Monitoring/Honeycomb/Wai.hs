{-# LANGUAGE TypeOperators #-}
module Network.Monitoring.Honeycomb.Wai where

import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Monitoring.Honeycomb
import Network.Monitoring.Honeycomb.Trace
import RIO

import qualified RIO.HashMap as HM

traceApplication'
    :: forall m env .
       ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasTracer env
       )
    => SpanName
    -> Application
    -> m Application
traceApplication' name app =
    withNewRootSpan name (const mempty) $ do
        u <- askUnliftIO
        pure $ \req inner ->
            app req ( \response -> do
                unliftIO u $ addToSpan $ getRequestFields req `HM.union` getResponseFields response
                inner response
                )
  where
    getRequestFields :: Request -> HoneyObject
    getRequestFields req = HM.fromList
        [ ("meta.span_type", HoneyString "http_request")
        , ("request.header.user_agent", maybe HoneyNil (toHoneyValue . decodeUtf8Lenient) (requestHeaderUserAgent req))
        , ("request.host", maybe HoneyNil (toHoneyValue . decodeUtf8Lenient) (requestHeaderHost req))
        , ("request.path", toHoneyValue . decodeUtf8Lenient $ rawPathInfo req)
        ]

    getResponseFields :: Response -> HoneyObject
    getResponseFields response = HM.fromList
        [ ("response.status_code", toHoneyValue . statusCode $ responseStatus response)
        ]
