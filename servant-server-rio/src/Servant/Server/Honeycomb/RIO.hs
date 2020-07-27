{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Servant.Server.Honeycomb.RIO
-- Description : Honeycomb-based tracing for Servant applications using the RIO monad
-- Copyright   : (c) 2020 Earnest Research
-- License     : Apache-2
-- Maintainer  : gcoady@earnestresearch.com
-- Stability   : alpha
-- Portability : POSIX
module Servant.Server.Honeycomb.RIO
  ( traceServerRIO,
    traceServerRIOWithContext,
    genericTraceServerRIO,
    genericTraceServerWithContextRIO,
  )
where

import Control.Monad.Except (ExceptT (..))
import Data.Kind (Type)
import qualified Honeycomb.Trace as HC
import Network.Wai.Honeycomb.Servant
import Network.Wai.UnliftIO (liftApplication, runApplicationT)
import RIO hiding (Handler)
import Servant
import Servant.API.Generic (AsApi, GenericServant, ToServant, ToServantApi)
import Servant.Server.Generic
import Servant.Server.Honeycomb

-- | Trace a RIO-based Servant service with Honeycomb.
--
-- Each request is placed in a trace, and reported to Honeycomb.
-- The current span context is also available within the service, so
-- it may be used to pass tracing information downstream to further
-- services.
traceServerRIO ::
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api '[],
    HasRequestInfo api
  ) =>
  -- | Honeycomb service name
  HC.ServiceName ->
  HC.SpanName ->
  -- | Proxy representing Servant API
  Proxy api ->
  -- | Implementation of service (in RIO monad)
  ServerT api (RIO env) ->
  RIO env Application
traceServerRIO service spanName proxy app =
  traceServer service spanName proxy app runService

traceServerRIOWithContext ::
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api context,
    HasRequestInfo api
  ) =>
  Context context ->
  -- | Honeycomb service name
  HC.ServiceName ->
  HC.SpanName ->
  -- | Proxy representing Servant API
  Proxy api ->
  -- | Implementation of service (in RIO monad)
  ServerT api (RIO env) ->
  RIO env Application
traceServerRIOWithContext context service spanName proxy app =
  traceServerWithContext context service spanName proxy app runService

genericTraceServerRIO ::
  forall (routes :: Type -> Type) (env :: Type) (api :: Type).
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api,
    GenericServant routes (AsServerT (RIO env)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) '[],
    ServerT (ToServantApi routes) (RIO env) ~ ToServant routes (AsServerT (RIO env))
  ) =>
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  routes (AsServerT (RIO env)) ->
  RIO env Application
genericTraceServerRIO service spanName api routes = do
  env <- ask
  runApplicationT
    . traceApplicationT service spanName api
    . liftApplication
    $ genericServeT (runService env) routes

genericTraceServerWithContextRIO ::
  forall (routes :: Type -> Type) (ctx :: [Type]) (env :: Type) (api :: Type).
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api,
    GenericServant routes (AsServerT (RIO env)),
    GenericServant routes AsApi,
    HasServer (ToServantApi routes) ctx,
    ServerT (ToServantApi routes) (RIO env) ~ ToServant routes (AsServerT (RIO env))
  ) =>
  Context ctx ->
  HC.ServiceName ->
  HC.SpanName ->
  Proxy api ->
  routes (AsServerT (RIO env)) ->
  RIO env Application
genericTraceServerWithContextRIO context service spanName api routes = do
  env <- ask
  runApplicationT
    . traceApplicationT service spanName api
    . liftApplication
    $ genericServeTWithContext (runService env) routes context

runService :: env -> RIO env a -> Handler a
runService env inner =
  Handler . ExceptT . try $ runRIO env inner
