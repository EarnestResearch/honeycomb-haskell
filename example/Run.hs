module Run (run) where

import Import
import Network.Monitoring.Honeycomb.Trace

innerProgram2 :: RIO App ()
innerProgram2 =
  withNewSpan "inner2" (const mempty) $ logInfo "We're inside the application!"

innerProgram :: RIO App ()
innerProgram = withNewSpan "inner" (const mempty) $
  void $ concurrently innerProgram2 innerProgram2

run :: RIO App ()
run = 
  let headers = [ ("X-Amzn-Trace-ID", "Root=1-5759e988-bd862e3fe1be46a994272793") ]
  in
    withNewSpan "outer" (const mempty) $
      void $ concurrently innerProgram innerProgram
