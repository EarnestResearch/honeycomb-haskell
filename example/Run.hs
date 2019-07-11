{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Honeycomb.Trace

import qualified RIO.Set as Set

innerProgram2 :: RIO App ()
innerProgram2 =
    withNewSpan' "inner2" $ do
        logInfo "We're inside the application!"

innerProgram :: RIO App ()
innerProgram = withNewSpan' "inner" $ do
    addField "test" ("test" :: Text)
    void $ concurrently innerProgram2 innerProgram2

run :: RIO App ()
run = 
    withNewRootSpan' "test_service" "outer" Nothing $
        withInheritableFields (const $ Set.singleton "test") $
          void $ concurrently innerProgram innerProgram
