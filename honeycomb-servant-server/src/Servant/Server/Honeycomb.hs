{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.Honeycomb
  ( Honeycomb,
    hoistHoneycombServer,
    hoistHoneycombServerWithContext,
    traceServer,
    traceServerWithContext,
  )
where

import Control.Monad.Reader (MonadReader, ask, local)
import Data.Kind (Type)
import qualified Honeycomb.Trace as HC
import Lens.Micro (over)
import Network.Wai.Honeycomb.Servant
import Servant
import Servant.Server.Internal.Delayed (passToServer)
import UnliftIO hiding (Handler)
import Servant.Honeycomb (HasRequestInfo)

data Honeycomb deriving (Typeable)

instance (HasServer api context) => HasServer (Honeycomb :> api) context where
  type ServerT (Honeycomb :> api) m = Maybe HC.SpanContext -> ServerT api m

  route Proxy context subserver =
    route
      (Proxy :: Proxy api)
      context
      (passToServer subserver spanContextFromRequest)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

hoistHoneycombServer ::
  ( MonadReader env m,
    HC.HasSpanContext env,
    HasServer api '[]
  ) =>
  Proxy api ->
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT (Honeycomb :> api) n
hoistHoneycombServer proxy =
  hoistHoneycombServerWithContext proxy (Proxy :: Proxy '[])

hoistHoneycombServerWithContext ::
  forall m n env api context.
  ( MonadReader env m,
    HC.HasSpanContext env,
    HasServer api context
  ) =>
  Proxy api ->
  Proxy context ->
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT (Honeycomb :> api) n
hoistHoneycombServerWithContext proxy proxycxt f server spanContext =
  hoistServerWithContext
    proxy
    proxycxt
    (f . local (over HC.spanContextL (const spanContext)))
    server

traceServer ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api '[],
    HasRequestInfo api
  ) =>
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  ServerT api m ->
  (forall x. env -> m x -> Handler x) ->
  m Application
traceServer proxy service spanName app f = do
  env <- ask
  tracingMiddleware <- traceApplication proxy service spanName
  pure $ tracingMiddleware
    . serve (Proxy :: Proxy (Honeycomb :> api))
    $ hoistHoneycombServer proxy (f env) app

traceServerWithContext ::
  forall m env ctx (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api ctx,
    HasRequestInfo api
  ) =>
  Context ctx ->
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  ServerT api m ->
  (forall x. env -> m x -> Handler x) ->
  m Application
traceServerWithContext context proxy service spanName app f = do
  env <- ask
  tracingMiddleware <- traceApplication proxy service spanName
  pure $ tracingMiddleware
    . serveWithContext (Proxy :: Proxy (Honeycomb :> api)) context
    $ hoistHoneycombServerWithContext proxy (Proxy :: Proxy ctx) (f env) app

-- WIP
-- genericTraceServer ::
--   forall (routes :: Type -> Type) (m :: Type -> Type) (env :: Type) (api :: Type).
--   ( MonadUnliftIO m,
--     MonadReader env m,
--     HC.HasHoney env,
--     HC.HasSpanContext env,
--     HasRequestInfo api,
--     GenericServant routes (AsServerT m),
--     GenericServant routes AsApi,
--     HasServer (ToServantApi routes) '[],
--     ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
--   ) =>
--   HC.ServiceName ->
--   HC.SpanName ->
--   routes (AsServerT m) ->
--   (forall x. m x -> Handler x) ->
--   m Application
-- genericTraceServer service spanName routes f = do
--   tracingMiddleware <- traceApplication (Proxy :: Proxy api) service spanName
--   pure $ tracingMiddleware . genericServeT f routes

-- genericTraceServerWithContext ::
--   forall (routes :: Type -> Type) (m :: Type -> Type) (ctx :: [Type]) (env :: Type) (api :: Type).
--   ( MonadUnliftIO m,
--     MonadReader env m,
--     HC.HasHoney env,
--     HC.HasSpanContext env,
--     HasRequestInfo api,
--     GenericServant routes (AsServerT m),
--     GenericServant routes AsApi,
--     HasServer (ToServantApi routes) ctx,
--     ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
--   ) =>
--   Context ctx ->
--   HC.ServiceName ->
--   HC.SpanName ->
--   routes (AsServerT m) ->
--   (forall x. m x -> Handler x) ->
--   m Application
-- genericTraceServerWithContext context service spanName routes f = do
--   tracingMiddleware <- traceApplication (Proxy :: Proxy api) service spanName
--   tracingMiddleware . genericServeTWithContext f routes context
