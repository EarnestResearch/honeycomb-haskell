{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ServantSpec where

import qualified Network.Wai as Wai
import Servant
import Servant.Honeycomb (RequestInfo (..), getRequestInfo)
import Test.Hspec

type Api1 = EmptyAPI

call1 = Wai.defaultRequest

type Api2 = GetNoContent

call2 = Wai.defaultRequest {Wai.requestMethod = "GET", Wai.pathInfo = [], Wai.rawPathInfo = "/"}

type Api3 = "hello" :> GetNoContent

call3 = Wai.defaultRequest {Wai.requestMethod = "GET", Wai.pathInfo = ["hello"], Wai.rawPathInfo = "/hello"}

type Api4 = "users" :> Capture "user_id" Int :> GetNoContent

call4 = Wai.defaultRequest {Wai.requestMethod = "GET", Wai.pathInfo = ["users", "100"], Wai.rawPathInfo = "/users/100"}

type Api5 =
  "users" :> Capture "user_id" Int :> GetNoContent
    :<|> "info" :> "me" :> PostNoContent

call5a = Wai.defaultRequest {Wai.requestMethod = "GET", Wai.pathInfo = ["users", "100"], Wai.rawPathInfo = "/users/100"}

call5b = Wai.defaultRequest {Wai.requestMethod = "POST", Wai.pathInfo = ["info", "me"], Wai.rawPathInfo = "/info/me"}

spec :: Spec
spec = describe "Servant" $ do
  it "should return no path for empty API" $
    getRequestInfo (Proxy :: Proxy Api1) call1
      `shouldBe` Nothing
  it "should blah" $
    getRequestInfo (Proxy :: Proxy Api2) call2
      `shouldBe` Just RequestInfo {pathSegments = [], capturedPathValues = [], queryParameters = []}
  it "should blah2" $
    getRequestInfo (Proxy :: Proxy Api3) call3
      `shouldBe` Just RequestInfo {pathSegments = ["hello"], capturedPathValues = [], queryParameters = []}
  it "should blah3" $
    getRequestInfo (Proxy :: Proxy Api4) call4
      `shouldBe` Just RequestInfo {pathSegments = ["users", ":user_id"], capturedPathValues = [("user_id", "100")], queryParameters = []}
  it "should blah4" $ do
    getRequestInfo (Proxy :: Proxy Api5) call5a
      `shouldBe` Just RequestInfo {pathSegments = ["users", ":user_id"], capturedPathValues = [("user_id", "100")], queryParameters = []}
    getRequestInfo (Proxy :: Proxy Api5) call5b
      `shouldBe` Just RequestInfo {pathSegments = ["info", "me"], capturedPathValues = [], queryParameters = []}
