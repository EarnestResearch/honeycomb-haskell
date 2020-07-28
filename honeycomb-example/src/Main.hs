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
import Network.HostName (getHostName)
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
import Servant.Client
import Servant.Client.Honeycomb.RIO
import Servant.Server.Honeycomb.RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

type FibonacciApi =
  "fib" :> Capture "index" Int :> Get '[JSON] Int
    :<|> "healthcheck" :> Get '[PlainText] Text

fibonacciApi :: Proxy FibonacciApi
fibonacciApi = Proxy

--- server implementations

-- Would it be more efficient to set up a lookup table?
-- Shut up, Dave.
runGetFibonacci :: Int -> RIO AppEnv Int
runGetFibonacci index
  | index <= 0 = throwIO $ err400 {errBody = "Fibonacci index must be greater than zero."}
  | index > 12 = throwIO $ err400 {errBody = "I'm afraid I can't let you do that, Dave."}
  | index <= 2 = pure 1
  | otherwise =
    uncurry (+)
      <$> concurrently
        (getFibonacci $ index - 1)
        (getFibonacci $ index - 2)

runGetHealthcheck :: RIO AppEnv Text
runGetHealthcheck = pure "ok"

fibonacciServer =
  runGetFibonacci
    :<|> runGetHealthcheck

-- client implementations

getFibonacci :<|> getHealthcheck =
  traceClientRIO Proxy fibonacciApi

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

defaultFieldsIO :: IO HC.HoneyObject
defaultFieldsIO = fmap (\h -> HC.HoneyObject $ HM.fromList [ ("meta.local_hostname", h) ]) $ HC.toHoneyValue <$> getHostName


main :: IO ()
main = HC.withHoney $ \honey -> do
  clientManager <- newManager defaultManagerSettings
  clientBaseUrl <- parseBaseUrl "http://localhost:3000/"
  defaultFields <- defaultFieldsIO
  let appEnv =
        AppEnv
          { aeSpanContext = Nothing,
            aeHoney = honey & HC.honeyOptionsL . HC.defaultFieldsL .~ pure defaultFields,
            aeClientEnv = mkClientEnv clientManager clientBaseUrl,
            aePort = 3000
          }
  app <- runRIO appEnv $ traceServerRIO fibonacciApi "infra-service" "http-handler" fibonacciServer
  run (aePort appEnv) app
