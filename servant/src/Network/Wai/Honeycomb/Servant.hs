{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wai.Honeycomb.Servant
  ( traceApplicationT,
    traceApplicationT',
    RequestInfo (..),
    HasRequestInfo,
    getRequestInfo,
    module Network.Wai.Honeycomb,
  )
where

import Control.Monad (MonadPlus (mplus))
import Control.Monad.Reader (MonadReader)
import Data.Kind (Type)
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Honeycomb.Trace as HC
import Lens.Micro.Mtl (view)
import qualified Network.Wai as Wai
import Network.Wai.Honeycomb (decodeUtf8Lenient, getRequestFields, getResponseFields, readTraceHeader, reportErrorStatus, spanContextKey)
import Network.Wai.UnliftIO (MiddlewareT)
import Servant
import UnliftIO (MonadUnliftIO, catchAny)

data RequestInfo
  = RequestInfo
      { pathSegments :: [T.Text],
        capturedPathValues :: [(T.Text, T.Text)],
        queryParameters :: [(T.Text, T.Text)]
      }
  deriving (Eq, Show, Generic)

traceApplicationT ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  HC.ServiceName ->
  Proxy api ->
  MiddlewareT m
traceApplicationT service proxy = traceApplicationT' service proxy readTraceHeader

traceApplicationT' ::
  forall m env (api :: Type).
  ( MonadUnliftIO m,
    MonadReader env m,
    HC.HasHoney env,
    HC.HasSpanContext env,
    HasRequestInfo api
  ) =>
  HC.ServiceName ->
  Proxy api ->
  (Wai.Request -> Maybe HC.SpanReference) ->
  MiddlewareT m
traceApplicationT' service proxy parentSpanRef app req inner =
  HC.withNewRootSpan' service "span" (parentSpanRef req) $ do
    let info = getRequestInfo proxy req
    spanContext <- view HC.spanContextL
    HC.add (getRequestFields req)
    (\x y -> app x y `catchAny` reportErrorStatus)
      req {Wai.vault = V.insert spanContextKey spanContext (Wai.vault req)}
      ( \response -> do
          HC.add (getResponseFields response)
          inner response
      )

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
