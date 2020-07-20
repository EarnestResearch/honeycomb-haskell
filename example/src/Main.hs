{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Honeycomb.Trace as HC
import Network.Wai.Handler.Warp (run)
import RIO hiding (Handler)
import Servant
import Servant.Server.Honeycomb.RIO

type Api =
  "hello" :> Capture "name" Text :> Get '[JSON] Text
    :<|> "goodbye" :> Capture "neverknewyou" Text :> Get '[PlainText] Text

helloName :: Text -> RIO env Text
helloName name = pure $ "hello " <> name

goodbyeWhoever :: Text -> RIO env Text
goodbyeWhoever whoever = pure $ "goodbye " <> whoever

appServer :: ServerT Api (RIO env)
appServer = helloName :<|> goodbyeWhoever

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
    app <- runRIO appEnv $ traceServerRIO "infra-service" "http-handler" (Proxy @Api) appServer
    run (aePort appEnv) app
