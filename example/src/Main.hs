{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Servant.API
import Servant.Honeycomb.RIO

type Api = "hello" :> Capture "int" Int :> Get '[JSON] Text

appServer :: Int -> RIO AppEnv Text
appServer i = do
  ctx <- view HC.spanContextL
  pure $ "hello " <> tshow i <> " " <> tshow ctx

data AppEnv
  = AppEnv
      { spanContext :: Maybe HC.SpanContext,
        honey :: HC.Honey,
        port :: Int
      }

instance HC.HasHoney AppEnv where
  honeyL = lens honey (\x y -> x {honey = y})

instance HC.HasSpanContext AppEnv where
  spanContextL = lens spanContext (\x y -> x {spanContext = y})

main :: IO ()
main =
  HC.withHoney $ \honey -> do
    let appEnv =
          AppEnv
            { spanContext = Nothing,
              honey = honey,
              port = 3000
            }
    app <- runRIO appEnv $ traceHoneycombRIO "infra-service" (Proxy @Api) appServer
    run (port appEnv) app
