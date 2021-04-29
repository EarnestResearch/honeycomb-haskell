{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
#if MIN_VERSION_servant_server(0,18,0)
import Servant.Server.Internal.ErrorFormatter (DefaultErrorFormatters, ErrorFormatters)
#endif
import Servant.Honeycomb (HasRequestInfo)

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
  -- | Proxy representing Servant API
  Proxy api ->
  -- | Honeycomb service name
  HC.ServiceName ->
  HC.SpanName ->
  -- | Implementation of service (in RIO monad)
  ServerT api (RIO env) ->
  RIO env Application
traceServerRIO proxy service spanName app =
  traceServer proxy service spanName app runService

traceServerRIOWithContext ::
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api ctx,
#if MIN_VERSION_servant_server(0,18,0)
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
#endif
    HasRequestInfo api
  ) =>
  Context ctx ->
  -- | Proxy representing Servant API
  Proxy api ->
  -- | Honeycomb service name
  HC.ServiceName ->
  HC.SpanName ->
  -- | Implementation of service (in RIO monad)
  ServerT api (RIO env) ->
  RIO env Application
traceServerRIOWithContext context proxy service spanName app =
  traceServerWithContext context proxy service spanName app runService

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
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  routes (AsServerT (RIO env)) ->
  RIO env Application
genericTraceServerRIO api service spanName routes = do
  env <- ask
  runApplicationT
    . traceApplicationT api service spanName
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
#if MIN_VERSION_servant_server(0,18,0)
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
#endif
    ServerT (ToServantApi routes) (RIO env) ~ ToServant routes (AsServerT (RIO env))
  ) =>
  Context ctx ->
  Proxy api ->
  HC.ServiceName ->
  HC.SpanName ->
  routes (AsServerT (RIO env)) ->
  RIO env Application
genericTraceServerWithContextRIO context api service spanName routes = do
  env <- ask
  runApplicationT
    . traceApplicationT api service spanName
    . liftApplication
    $ genericServeTWithContext (runService env) routes context

runService :: env -> RIO env a -> Handler a
runService env inner =
  Handler . ExceptT . try $ runRIO env inner
