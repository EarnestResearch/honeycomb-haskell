{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Server.Honeycomb.RIO

data Routes route
  = Routes
      { _hello :: route :- "hello" :> Capture "name" Text :> Get '[JSON] Text,
        _goodbye :: route :- "goodbye" :> Capture "neverknewyou" Text :> Get '[PlainText] Text
      }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

serverImpl :: Routes (AsServerT (RIO AppEnv))
serverImpl =
  Routes
    { _hello = \name -> pure $ "hello " <> name,
      _goodbye = \name -> pure $ "goodbye " <> name
    }

data AppEnv
  = AppEnv
      { aeSpanContext :: Maybe HC.SpanContext,
        aeHoney :: HC.Honey,
        aePort :: Int
      }

instance HC.HasHoney AppEnv where
  honeyL = lens aeHoney (\x y -> x {aeHoney = y})

instance HC.HasSpanContext AppEnv where
  spanContextL = lens aeSpanContext (\x y -> x {aeSpanContext = y})

main :: IO ()
main =
  HC.withHoney $ \honey -> do
    let appEnv =
          AppEnv
            { aeSpanContext = Nothing,
              aeHoney = honey,
              aePort = 3000
            }
    app <- runRIO appEnv $ genericTraceServerRIO "infra-service" "http-handler" api serverImpl
    run (aePort appEnv) app
