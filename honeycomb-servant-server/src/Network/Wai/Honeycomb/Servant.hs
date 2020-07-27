{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Honeycomb.Servant
  ( traceApplication,
    traceApplicationT,
    module Network.Wai.Honeycomb,
  )
where

import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import qualified Honeycomb.Trace as HC
import qualified Network.Wai as Wai
import Network.Wai.Honeycomb hiding (traceApplication, traceApplication', traceApplicationT, traceApplicationT')
import qualified Network.Wai.Honeycomb as HCWai
import Network.Wai.UnliftIO
import Servant
import Servant.Honeycomb
import UnliftIO
import Network.Wai (Middleware)

traceApplication ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  m Middleware
traceApplication proxy service spanName =
  HCWai.traceApplication' service spanName extraReqDetails
  where
    extraReqDetails :: Wai.Request -> Maybe HC.HoneyObject
    extraReqDetails r = HC.toHoneyObject <$> getRequestInfo proxy r

traceApplicationT ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  MiddlewareT m
traceApplicationT proxy service spanName =
  HCWai.traceApplicationT' service spanName extraReqDetails
  where
    extraReqDetails :: Wai.Request -> Maybe HC.HoneyObject
    extraReqDetails r = HC.toHoneyObject <$> getRequestInfo proxy r
