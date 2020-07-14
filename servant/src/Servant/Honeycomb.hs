{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Honeycomb
  ( Honeycomb,
    RequestInfo (..),
    getRequestInfo,
    runTraceHandler,
  )
where

import Control.Monad (mplus)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (bimap)
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Honeycomb.Trace as HC
import qualified Network.Wai as Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)
import UnliftIO

data RequestInfo
  = RequestInfo
      { pathSegments :: [T.Text],
        capturedPathValues :: [(T.Text, T.Text)],
        queryParameters :: [(T.Text, T.Text)]
      }
  deriving (Eq, Show, Generic)

data TraceHandlerData = TraceHandlerData HC.HoneyObject (Maybe HC.SpanReference)

runTraceHandler :: (MonadUnliftIO m, MonadReader env m, HC.HasHoney env, HC.HasSpanContext env) => TraceHandlerData -> m a -> m a
runTraceHandler (TraceHandlerData ho spanRef) inner =
  HC.withNewRootSpan' "infra-app" (HC.SpanName "span") spanRef $ HC.add ho >> inner

fillData :: Maybe RequestInfo -> TraceHandlerData
fillData info =
  let path = T.intercalate "/" . pathSegments <$> info
      pathParams = bimap ("request.path_param." <>) HC.toHoneyValue <$> maybe [] capturedPathValues info
   in TraceHandlerData
        (HC.toHoneyObject $ ("request.path", HC.toHoneyValue path) : pathParams)
        Nothing

data Honeycomb deriving (Typeable)

instance (HasRequestInfo api, HasServer api context) => HasServer (Honeycomb :> api) context where
  type ServerT (Honeycomb :> api) m = TraceHandlerData -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $ passToServer subserver $ fillData . getRequestInfo (Proxy :: Proxy api)

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

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
        RequestInfo {pathSegments, capturedPathValues, queryParameters} <- getRequestInfo (Proxy :: Proxy sub) req {Wai.pathInfo = tl}
        Just RequestInfo {pathSegments = hd : pathSegments, capturedPathValues, queryParameters}
      _ -> Nothing

instance (KnownSymbol (capture :: Symbol), HasRequestInfo (sub :: Type)) => HasRequestInfo (Capture' mods capture a :> sub) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      (hd : tl) -> do
        RequestInfo {pathSegments, capturedPathValues, queryParameters} <- getRequestInfo (Proxy :: Proxy sub) req {Wai.pathInfo = tl}
        let newSymbol = symbolVal (Proxy :: Proxy capture)
        pure RequestInfo {pathSegments = T.pack (':' : newSymbol) : pathSegments, capturedPathValues = (T.pack newSymbol, hd) : capturedPathValues, queryParameters}
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
      [] | Wai.requestMethod req == method -> Just (RequestInfo {pathSegments = [], capturedPathValues = [], queryParameters = []})
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasRequestInfo (NoContentVerb method) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      [] | Wai.requestMethod req == method -> Just (RequestInfo {pathSegments = [], capturedPathValues = [], queryParameters = []})
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasRequestInfo (Stream method status framing ct a) where
  getRequestInfo _ req =
    case Wai.pathInfo req of
      [] | Wai.requestMethod req == method -> Just (RequestInfo {pathSegments = [], capturedPathValues = [], queryParameters = []})
      _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasRequestInfo Raw where
  getRequestInfo _ _ = Just (RequestInfo [] [] [])

instance HasRequestInfo (sub :: Type) => HasRequestInfo (CaptureAll (h :: Symbol) a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)

instance HasRequestInfo (sub :: Type) => HasRequestInfo (BasicAuth (realm :: Symbol) a :> sub) where
  getRequestInfo _ = getRequestInfo (Proxy :: Proxy sub)
