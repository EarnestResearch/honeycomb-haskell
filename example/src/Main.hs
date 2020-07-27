{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Honeycomb.Trace as HC
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Servant.Client.Honeycomb
import Servant.Server.Generic
import Servant.Server.Honeycomb.RIO

type FibonacciApi = "fib" :> Capture "index" Int :> Get '[JSON] Int

fibonacciServer :: ServerT FibonacciApi (RIO AppEnv)
fibonacciServer index =
  if index <= 2
    then pure index
    else do
      v1 <- getFibonacci (index - 1)
      v2 <- getFibonacci (index - 2)
      pure (v1 + v2)

fibonacciApi :: Proxy FibonacciApi
fibonacciApi = Proxy

getFibonacci :: Int -> RIO AppEnv Int
getFibonacci =
  hoistClient fibonacciApi (RIO . unTraceClientM) $ traceClient fibonacciApi (Proxy :: Proxy (TraceClientM AppEnv))

--- generic

-- newtype Routes route = Routes
--   { _fibonacci :: route :- "fib" :> Capture "index" Int :> Get '[JSON] Int
--   }
--   deriving (Generic)

-- api :: Proxy (ToServantApi Routes)
-- api = genericApi (Proxy :: Proxy Routes)

-- serverImpl :: Routes (AsServerT (RIO AppEnv))
-- serverImpl =
--   Routes
--     { _fibonacci = \index ->
--         if index <= 2
--           then pure index
--           else do
--             v1 <- _fibonacci clientRoutes (index - 1)
--             v2 <- _fibonacci clientRoutes (index - 2)
--             pure (v1 + v2)
--     }

-- clientRoutes :: (HC.HasSpanContext env, HasClientEnv env) => Routes (AsClientT (RIO env))
-- clientRoutes = genericClientHoist $ RIO . unTraceClientM

---

data AppEnv = AppEnv
  { aeSpanContext :: Maybe HC.SpanContext,
    aeHoney :: HC.Honey,
    aeClientEnv :: ClientEnv,
    aePort :: Int
  }

instance HasClientEnv AppEnv where
  clientEnvL = lens aeClientEnv (\x y -> x {aeClientEnv = y})

instance HC.HasHoney AppEnv where
  honeyL = lens aeHoney (\x y -> x {aeHoney = y})

instance HC.HasSpanContext AppEnv where
  spanContextL = lens aeSpanContext (\x y -> x {aeSpanContext = y})

main :: IO ()
main = HC.withHoney $ \honey -> do
  clientManager <- newManager defaultManagerSettings
  clientBaseUrl <- parseBaseUrl "http://localhost:3000/"
  let appEnv =
        AppEnv
          { aeSpanContext = Nothing,
            aeHoney = honey,
            aeClientEnv = mkClientEnv clientManager clientBaseUrl,
            aePort = 3000
          }
  app <- runRIO appEnv $ traceServerRIO "infra-service" "http-handler" fibonacciApi fibonacciServer
  run (aePort appEnv) app
