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
    TraceHandlerData (..),
    hoistHoneycombServer,
    hoistHoneycombServerWithContext,
    traceServer,
    traceServerWithContext,
  )
where

import Control.Monad (join)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Kind (Type)
import qualified Data.Vault.Lazy as V
import qualified Honeycomb.Trace as HC
import Lens.Micro (over)
import qualified Network.Wai as Wai
import Network.Wai.Honeycomb.Servant
import Servant
import Servant.Server.Internal.Delayed (passToServer)
import UnliftIO hiding (Handler)
import UnliftIO.Wai

data TraceHandlerData = TraceHandlerData Wai.Request (Maybe RequestInfo)

data Honeycomb deriving (Typeable)

instance (HasServer api context) => HasServer (Honeycomb :> api) context where
  type ServerT (Honeycomb :> api) m = Maybe HC.SpanContext -> ServerT api m

  route Proxy context subserver =
    route
      (Proxy :: Proxy api)
      context
      ( passToServer
          subserver
          (join . V.lookup spanContextKey . Wai.vault)
      )
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
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  ServerT api m ->
  (forall x. env -> m x -> Handler x) ->
  m Application
traceServer service spanName proxy app f = do
  env <- ask
  runApplicationT
    $ traceApplicationT service spanName proxy
    $ liftApplication
    $ serve (Proxy :: Proxy (Honeycomb :> api))
    $ hoistHoneycombServer proxy (f env) app

traceServerWithContext ::
  forall m env context (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api context,
    HasRequestInfo api
  ) =>
  Context context ->
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  ServerT api m ->
  (forall x. env -> m x -> Handler x) ->
  m Application
traceServerWithContext context service spanName proxy app f = do
  env <- ask
  runApplicationT
    $ traceApplicationT service spanName proxy
    $ liftApplication
    $ serveWithContext (Proxy :: Proxy (Honeycomb :> api)) context
    $ hoistHoneycombServerWithContext proxy (Proxy :: Proxy context) (f env) app
-- WORK NEEDED
-- genericTraceServer ::
--   forall m env (api :: Type).
--   ( MonadUnliftIO m,
--     MonadReader env m,
--     HC.HasHoney env,
--     HC.HasSpanContext env,
--     HasServer api '[],
--     HasRequestInfo api
--   ) =>
--   HC.ServiceName ->
--   HC.SpanName ->
--   Proxy api ->
--   ServerT api m ->
--   (forall x. env -> m x -> Handler x) ->
--   m Application
-- genericTraceServer service spanName proxy app f = do
--   env <- ask
--   runApplicationT
--     $ traceApplicationT service spanName proxy
--     $ liftApplication
--     $ genericServeT (f env) (Proxy :: Proxy (Honeycomb :> api))
--     $ hoistHoneycombServer proxy (f env) app
