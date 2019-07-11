{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import Import
import Network.Monitoring.Honeycomb
import Run
import RIO.Process
import Options.Applicative.Simple

import qualified Paths_honeycomb
import qualified RIO.Text as Text

main :: IO ()
main = do
    (appOptions, ()) <- simpleOptions
      $(simpleVersion Paths_honeycomb.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      (Options
        <$> switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                    )
        <*> strArgument ( metavar "APIKEY"
                       <> help "Honeycomb API Key"
                        )
      )
      empty
    lo <- logOptionsHandle stderr (optionsVerbose appOptions)
    appProcessContext <- mkDefaultProcessContext
    let ho = defaultHoneyOptions
               & apiKeyL ?~ ApiKey (Text.pack (optionsApiKey appOptions))
               & datasetL ?~ "tracing-development"
    withLogFunc lo $ \appLogFunc ->
      withHoney defaultHoneyServerOptions ho $ \appHoney ->
        let app = App
              { appLogFunc
              , appProcessContext
              , appOptions
              , appHoney
              , appSpanContext = Nothing
              }
        in runRIO app run
