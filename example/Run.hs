{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Network.Monitoring.Honeycomb.Trace

innerProgram2 :: RIO App ()
innerProgram2 =
  withNewSpan "inner2" (const mempty) $ do
    addFieldToSpan "test" ("test" :: Text)
    logInfo "We're inside the application!"

innerProgram :: RIO App ()
innerProgram = withNewSpan "inner" (const mempty) $
  void $ concurrently innerProgram2 innerProgram2

run :: RIO App ()
run = 
  withNewSpan "outer" (const mempty) $
    void $ concurrently innerProgram innerProgram
