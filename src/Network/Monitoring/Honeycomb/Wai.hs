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

type Application' m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived

traceApplication'
    :: forall m env .
       ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasTracer env
       )
    => SpanName
    -> Application
    -> Application' m
traceApplication' name app req inner =
    withNewRootSpan name (const mempty) $ do
        --curFields <- preview $ tracerL . spanContextL . _Just . spanEventL . eventFieldsL
        --frozen <- maybe (pure HM.empty) readTVarIO curFields
        --liftIO $ print $ "Cur fields: " <> show frozen
        addToSpan getRequestFields
        withRunInIO $ \runInIO -> app req (runInIO . inner)
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
