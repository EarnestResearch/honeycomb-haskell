{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Honeycomb
  ( RequestInfo (..),
    HasRequestInfo,
    getRequestInfo,
  )
where

import Control.Monad (MonadPlus (mplus))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types.Method
import qualified Honeycomb.Trace as HC
import qualified Network.Wai as Wai
import Servant
import Servant.Auth (Auth)
import Debug.Trace (trace)

data RequestInfo
  = RequestInfo
      { pathSegments :: ![T.Text],
        pathParameters :: ![(T.Text, T.Text)]
      }
  deriving (Eq, Show, Generic)

instance HC.ToHoneyObject RequestInfo where
  toHoneyObject ri =
    HC.HoneyObject
      $ HM.fromList
      $ [ ("handler.route", HC.toHoneyValue . ("/" <>) . T.intercalate "/" $ pathSegments ri)
        ]
        <> (bimap ("handler.route_param." <>) HC.toHoneyValue <$> pathParameters ri)

class HasRequestInfo a where
  getRequestInfo :: Proxy a -> Wai.Request -> Maybe RequestInfo

instance HasRequestInfo EmptyAPI where
  getRequestInfo _ _ = Nothing

instance (HasRequestInfo (a :: Type), HasRequestInfo (b :: Type)) => HasRequestInfo (a :<|> b) where
  getRequestInfo _ req =
    getRequestInfo (Proxy :: Proxy a) req
      `mplus` getRequestInfo (Proxy :: Proxy b) req

instance (KnownSymbol (path :: Symbol), HasRequestInfo (sub :: Type)) => HasRequestInfo (path :> sub) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      (hd : tl) | hd == T.pack (symbolVal (Proxy :: Proxy path)) -> do
        RequestInfo {pathSegments, pathParameters} <- getRequestInfo (Proxy :: Proxy sub) req {Wai.pathInfo = tl}
        Just RequestInfo {pathSegments = hd : pathSegments, pathParameters}
      _ -> Nothing

instance (KnownSymbol (capture :: Symbol), HasRequestInfo (sub :: Type)) => HasRequestInfo (Capture' mods capture a :> sub) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      (hd : tl) -> do
        RequestInfo {pathSegments, pathParameters} <- getRequestInfo (Proxy :: Proxy sub) req {Wai.pathInfo = tl}
        let newSymbol = symbolVal (Proxy :: Proxy capture)
        pure RequestInfo {pathSegments = T.pack (':' : newSymbol) : pathSegments, pathParameters = (T.pack newSymbol, hd) : pathParameters}
      _ -> Nothing

instance HasRequestInfo (sub :: Type) => HasRequestInfo (Summary d :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (Description d :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (Header' mods h a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (QueryParam' mods (h :: Symbol) a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (QueryParams (h :: Symbol) a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (QueryFlag h :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (ReqBody' mods cts a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (StreamBody' mods framing ct a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (RemoteHost :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (IsSecure :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (HttpVersion :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (Vault :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (WithNamedContext x y sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance ReflectMethod method => HasRequestInfo (Verb method status cts a) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      [] -> if Wai.requestMethod req == method || (Wai.requestMethod req == methodHead && method == methodGet)
              then Just (RequestInfo {pathSegments = [], pathParameters = []})
            else Nothing
      stuff -> trace (show stuff) Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

#if MIN_VERSION_servant(0,17,0)
instance ReflectMethod method => HasRequestInfo (NoContentVerb method) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      [] -> if Wai.requestMethod req == method || (Wai.requestMethod req == methodHead && method == methodGet)
              then Just (RequestInfo {pathSegments = [], pathParameters = []})
            else Nothing
      stuff -> trace (show stuff) Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)
#endif

instance ReflectMethod method => HasRequestInfo (Stream method status framing ct a) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      [] -> if Wai.requestMethod req == method || (Wai.requestMethod req == methodHead && method == methodGet)
              then Just (RequestInfo {pathSegments = [], pathParameters = []})
            else Nothing
      stuff -> trace (show stuff) Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasRequestInfo Raw where
  getRequestInfo _ _ = Just (RequestInfo{pathSegments = ["RAW"], pathParameters = []})

instance HasRequestInfo (sub :: Type) => HasRequestInfo (CaptureAll (h :: Symbol) a :> sub) where
  getRequestInfo _ req = getRequestInfo (Proxy :: Proxy sub) req{Wai.pathInfo = []}

instance HasRequestInfo (sub :: Type) => HasRequestInfo (BasicAuth (realm :: Symbol) a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (Auth auths v :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)
