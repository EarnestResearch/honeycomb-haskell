{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  )
where

import Control.Monad.Except (ExceptT (..))
import qualified Honeycomb.Trace as HC
import Network.Wai.Honeycomb.Servant
import RIO hiding (Handler)
import Servant
import Servant.Server.Honeycomb

-- | Trace a RIO-based Servant service with Honeycomb.
--
-- Each request is placed in a trace, and reported to Honeycomb.
-- The current span context is also available within the service, so
-- it may be used to pass tracing information downstream to further
-- services.
traceServerRIO ::
  forall env api.
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
  forall env api context.
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

runService :: env -> RIO env a -> Handler a
runService env inner =
  Handler . ExceptT . try $ runRIO env inner
