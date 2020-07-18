{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Servant.Honeycomb.RIO
  ( traceHoneycombRIO,
  )
where

import qualified Honeycomb.Trace as HC
import Network.Wai.Honeycomb.Servant
import RIO hiding (Handler)
import Servant
import Servant.Honeycomb

traceHoneycombRIO ::
  forall env api.
  ( HC.HasHoney env,
    HC.HasSpanContext env,
    HasServer api '[],
    HasRequestInfo api
  ) =>
  HC.ServiceName ->
  Proxy api ->
  ServerT api (RIO env) ->
  RIO env Application
traceHoneycombRIO service proxy app =
  traceHoneycomb service proxy app runRIO
