{-# LANGUAGE OverloadedStrings #-}

module HoneycombSpec where

import Clients
import Data.Dynamic (fromDynamic)
import qualified Honeycomb as HC
import Lens.Micro
import RIO
import qualified RIO.Text as T
import Test.Hspec

withSuccessfulHoneyClient ::
  RIO HC.Honey a ->
  IO a
withSuccessfulHoneyClient prg =
  HC.withHoney' (HC.defaultHoneyTestServerOptions multipleSuccessfulResponses) honeyOptions $ \client ->
    runRIO client prg
  where
    -- API requires API key and dataset to be set
    honeyOptions :: HC.HoneyOptions
    honeyOptions = HC.defaultHoneyOptions & HC.apiKeyL ?~ "apikey" & HC.datasetL ?~ "dataset"

sendAndFlushEvent :: HC.HoneyEvent -> RIO HC.Honey ()
sendAndFlushEvent evt = do
  HC.send evt
  HC.flush 100000

spec :: Spec
spec =
  describe "send" $ do
    context "when a single successful event is sent"
      $ it "response status code is 202"
      $ withSuccessfulHoneyClient
      $ do
        q <- view HC.honeyResponseQueueL
        evt <- HC.newEventWithMetadata ((), ())
        _ <- HC.addField "key" (HC.HoneyString "value") evt
        sendAndFlushEvent evt
        firstItem <- atomically $ readTBQueue q
        isNowEmpty <- atomically $ isEmptyTBQueue q
        liftIO $ do
          isNowEmpty `shouldBe` True
          HC.honeyResponseStatusCode firstItem `shouldBe` Just 202
          HC.honeyResponseError firstItem `shouldBe` Nothing
          (HC.honeyResponseMetadata firstItem >>= fromDynamic) `shouldBe` Just ((), ())
          (HC.honeyResponseMetadata firstItem >>= fromDynamic) `shouldNotBe` Just ((), (), ())
    context "when an overlarge message is sent"
      $ it "overlarge failure message is sent"
      $ withSuccessfulHoneyClient
      $ do
        q <- view HC.honeyResponseQueueL
        evt <- HC.newEventWithMetadata ((), ())
        _ <- HC.addField "key" (HC.HoneyString $ T.replicate 120000 "a") evt
        sendAndFlushEvent evt
        firstItem <- atomically $ readTBQueue q
        liftIO $ do
          HC.honeyResponseStatusCode firstItem `shouldBe` Nothing
          HC.honeyResponseError firstItem `shouldBe` Just "Oversized event sent to API"
          (HC.honeyResponseMetadata firstItem >>= fromDynamic) `shouldBe` Just ((), ())
          (HC.honeyResponseMetadata firstItem >>= fromDynamic) `shouldNotBe` Just ((), (), ())
    context "when an overlarge message and normal message is sent"
      $ it "overlarge failure message is sent and normal result is sent"
      $ withSuccessfulHoneyClient
      $ do
        q <- view HC.honeyResponseQueueL
        evt1 <- HC.newEventWithMetadata ((), ())
        _ <- HC.addField "key" (HC.HoneyString "value") evt1
        sendAndFlushEvent evt1
        evt2 <- HC.newEventWithMetadata ((), (), ())
        _ <- HC.addField "key" (HC.HoneyString $ T.replicate 120000 "a") evt2
        sendAndFlushEvent evt2
        firstItem <- atomically $ readTBQueue q
        secondItem <- atomically $ readTBQueue q
        isNowEmpty <- atomically $ isEmptyTBQueue q
        liftIO $ do
          isNowEmpty `shouldBe` True
          (HC.honeyResponseMetadata firstItem >>= fromDynamic) `shouldBe` Just ((), ())
          HC.honeyResponseStatusCode firstItem `shouldBe` Just 202
          HC.honeyResponseError firstItem `shouldBe` Nothing
          (HC.honeyResponseMetadata secondItem >>= fromDynamic) `shouldBe` Just ((), (), ())
          HC.honeyResponseStatusCode secondItem `shouldBe` Nothing
          HC.honeyResponseError secondItem `shouldBe` Just "Oversized event sent to API"
