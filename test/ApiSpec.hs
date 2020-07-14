{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiSpec where

import Clients
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import Honeycomb.Api
import qualified Network.HTTP.Client.Internal as Client
import RIO
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
      context "when oversized event is provided"
        $ it "should return event in oversized result"
        $ do
          SendEventsResponse {oversizedEvents} <- sendEvents multipleSuccessfulResponses requestOptions [oversizedEvent]
          List.length oversizedEvents `shouldBe` 1
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
    context "when request always results in response timeout"
      $ it "makes two calls before failure"
      $ do
        numCallsRef <- newIORef (0 :: Int)
        result <-
          tryAny $
            sendEvents
              (httpLbsCallCountOnError numCallsRef (\_ req -> Left $ Client.HttpExceptionRequest req Client.ResponseTimeout))
              requestOptions
              [newEvent]
        numCalls <- readIORef numCallsRef
        numCalls `shouldBe` 2
        isLeft result `shouldBe` True
    context "when request results in response timeout first time"
      $ it "makes two calls before success"
      $ do
        numCallsRef <- newIORef (0 :: Int)
        result <-
          tryAny $
            sendEvents
              ( httpLbsCallCountOnError
                  numCallsRef
                  ( \num req ->
                      if num == 1
                        then Left $ Client.HttpExceptionRequest req Client.ResponseTimeout
                        else Right undefined
                  )
              )
              requestOptions
              [newEvent]
        numCalls <- readIORef numCallsRef
        numCalls `shouldBe` 2
        isRight result `shouldBe` True

-- Request options used for requests
requestOptions :: RequestOptions
requestOptions = mkRequestOptions (ApiHost "https://api.honeycomb.io/") (Dataset "") (ApiKey "")

{-
    Some sample events.
-}
newEvent :: Event
newEvent = mkEvent (HoneyObject $ HM.singleton "key" "value")

oversizedEvent :: Event
oversizedEvent = mkEvent (HoneyObject . HM.singleton "key" . HoneyString $ T.replicate 120000 "a")

{-
    Send a list of events, and verify that the body can be read as a [Event].
    Also count the number of requests made.
-}
sendEventsAndCheckJSON :: IORef [Maybe [Event]] -> [Event] -> IO SendEventsResponse
sendEventsAndCheckJSON requestsRef =
  sendEvents (httpLbsJSONRequests requestsRef) requestOptions

httpLbsJSONRequests :: IORef [Maybe [Event]] -> Client.Request -> IO (Client.Response LBS.ByteString)
httpLbsJSONRequests ref req = do
  case Client.requestBody req of
    Client.RequestBodyLBS b ->
      modifyIORef ref (<> [JSON.decode b :: Maybe [Event]])
    _ ->
      pure ()
  multipleSuccessfulResponses req

{-
    Cause the error (or response) provided by the caller.
    Also count the number of requests made.
-}
httpLbsCallCountOnError :: IORef Int -> (Int -> Client.Request -> Either Client.HttpException (Client.Response LBS.ByteString)) -> Client.Request -> IO (Client.Response LBS.ByteString)
httpLbsCallCountOnError ref makeResponse req = do
  modifyIORef ref (+ 1)
  curVal <- readIORef ref
  let result = makeResponse curVal req
  either throwIO pure result

{-
    Count the number of requests, and return empty response.
-}
httpLbsCallCount :: IORef Int -> Client.Request -> IO (Client.Response LBS.ByteString)
httpLbsCallCount ref req = do
  modifyIORef ref (+ 1)
  multipleSuccessfulResponses req
