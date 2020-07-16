{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import RIO
import Servant
import qualified Prelude as P

type Api = "hello" :> Capture "int" Int :> GetNoContent

helloHandler :: Int -> IO ()
helloHandler _ = pure ()

api :: Proxy Api
api = Proxy

app = hoistServer api id helloHandler

app = serve @Api Proxy helloHandler

main :: IO ()
main = P.putStrLn "Hello, Haskell!"
