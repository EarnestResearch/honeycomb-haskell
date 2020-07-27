{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Honeycomb.Servant
  ( traceApplicationT,
    traceApplicationT',
    RequestInfo (..),
    HasRequestInfo,
    getRequestInfo,
    module Network.Wai.Honeycomb,
  )
where

import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import qualified Data.Vault.Lazy as V
import qualified Honeycomb.Trace as HC
import Lens.Micro.Mtl (view)
import qualified Network.Wai as Wai
import Network.Wai.Honeycomb hiding (traceApplicationT)
import Network.Wai.UnliftIO
import Servant
import Servant.Honeycomb
import UnliftIO

traceApplicationT ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  MiddlewareT m
traceApplicationT service spanName proxy = traceApplicationT' service spanName proxy readTraceHeader

traceApplicationT' ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  (Wai.Request -> Maybe HC.SpanReference) ->
  MiddlewareT m
traceApplicationT' service spanName proxy parentSpanRef app req inner =
  let info = getRequestInfo proxy req
   in HC.withNewRootSpan' service spanName (parentSpanRef req) $ do
        spanContext <- view HC.spanContextL
        HC.add (getRequestFields req)
        (\x y -> app x y `catchAny` reportErrorStatus)
          req {Wai.vault = V.insert spanContextKey spanContext (Wai.vault req)}
          ( \response -> do
              HC.add (getResponseFields response)
              maybe (pure ()) (HC.add . HC.toHoneyObject) info
              inner response
          )
