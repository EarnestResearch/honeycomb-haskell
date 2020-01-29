{-# LANGUAGE OverloadedStrings #-}

module ApiSpec where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as List
import Data.Maybe (isJust)
import Honeycomb.Api
import qualified Network.HTTP.Client as Client
import Test.Hspec

spec :: Spec
spec =
  describe "sendEvents" $ do
    context "when zero events are provided"
      $ xit "makes zero calls"
      $ do
        numCallsRef <- newIORef (0 :: Int)
        _ <- sendEvents (httpLbsCallCount numCallsRef) requestOptions []
        numCalls <- readIORef numCallsRef
        numCalls `shouldBe` 0
    context "when one event is provided" $ do
      it "makes one call" $ do
        numCallsRef <- newIORef (0 :: Int)
        _ <- sendEvents (httpLbsCallCount numCallsRef) requestOptions [newEvent]
        numCalls <- readIORef numCallsRef
        numCalls `shouldBe` 1
      it "should be a valid JSON request" $ do
        jsonsRef <- newIORef []
        _ <- sendEventsAndCheckJSON jsonsRef [newEvent]
        jsons <- readIORef jsonsRef
        List.all isJust jsons `shouldBe` True
    context "when two events is provided" $ do
      it "makes one call" $ do
        numCallsRef <- newIORef (0 :: Int)
        _ <- sendEvents (httpLbsCallCount numCallsRef) requestOptions [newEvent, newEvent]
        numCalls <- readIORef numCallsRef
        numCalls `shouldBe` 1
      it "should be valid JSON" $ do
        jsonsRef <- newIORef []
        _ <- sendEventsAndCheckJSON jsonsRef [newEvent, newEvent]
        jsons <- readIORef jsonsRef
        List.all isJust jsons `shouldBe` True
  where
    sendEventsAndCheckJSON :: IORef [Maybe [Event]] -> [Event] -> IO (Client.Response (Maybe [Integer]))
    sendEventsAndCheckJSON requestsRef =
      sendEvents (httpLbsJSONRequests requestsRef) requestOptions
    requestOptions :: RequestOptions
    requestOptions = mkRequestOptions (ApiHost "https://api.honeycomb.io/") (Dataset "") (ApiKey "")
    newEvent :: Event
    newEvent = mkEvent (HM.singleton "key" "value") Nothing Nothing
    httpLbsCallCount :: IORef Int -> Client.Request -> IO (Client.Response LBS.ByteString)
    httpLbsCallCount ref _ = do
      modifyIORef ref (+ 1)
      pure undefined
    httpLbsJSONRequests :: IORef [Maybe [Event]] -> Client.Request -> IO (Client.Response LBS.ByteString)
    httpLbsJSONRequests ref req = do
      case Client.requestBody req of
        Client.RequestBodyLBS b -> modifyIORef ref (<> [JSON.decode b :: Maybe [Event]]) -- O(N^2) I know
        _ -> pure ()
      pure undefined
