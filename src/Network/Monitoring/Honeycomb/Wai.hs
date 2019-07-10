{-# LANGUAGE TypeOperators #-}
module Network.Monitoring.Honeycomb.Wai where

import Lens.Micro (_Just)
import Lens.Micro.Mtl (preview)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Monitoring.Honeycomb
import Network.Monitoring.Honeycomb.Trace
import RIO
import System.IO (print)

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
        curFields <- preview $ tracerL . spanContextL . _Just . spanEventL . eventFieldsL
        frozen <- maybe (pure HM.empty) readTVarIO curFields
        liftIO $ print $ "Cur fields: " <> show frozen
        pure $ \req inner -> do
            unliftIO u $ addToSpan $ getRequestFields req
            curFields' <- unliftIO u $ preview $ tracerL . spanContextL . _Just . spanEventL . eventFieldsL
            frozen' <- maybe (pure HM.empty) readTVarIO curFields'
            print $ "New cur fields: " <> show frozen'
            app req inner
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
