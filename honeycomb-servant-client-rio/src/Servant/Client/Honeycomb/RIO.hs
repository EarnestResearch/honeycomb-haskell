{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Client.Honeycomb.RIO
  ( traceClientRIO,
    HasClientEnv,
    clientEnvL,
  )
where

import RIO
import Servant.Client (Client, ClientM, HasClient, hoistClient)
import Servant.Client.Core (clientIn)
import Servant.Client.Honeycomb (HasClientEnv, TraceClientM, clientEnvL, unTraceClientM)

traceClientRIO ::
  forall api env.
  ( HasClient ClientM api,
    HasClient (TraceClientM env) api
  ) =>
  Proxy env ->
  Proxy api ->
  Client (RIO env) api
traceClientRIO _ api =
  hoistClient api toRIO $
    api `clientIn` (Proxy :: Proxy (TraceClientM env))
  where
    toRIO :: TraceClientM env a -> RIO env a
    toRIO = RIO . unTraceClientM
