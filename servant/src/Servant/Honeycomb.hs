{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  ( Honeycomb,
    TraceHandlerData (..),
    RequestInfo (..),
    getRequestInfo,
    hoistHoneycombServer,
    hoistHoneycombServerWithContext,
  )
where

import Control.Monad (mplus)
import Control.Monad.Reader (MonadReader, local)
import Data.Kind (Type)
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Honeycomb.Trace as HC
import Lens.Micro (over)
import qualified Network.Wai as Wai
import Network.Wai.Honeycomb (spanContextKey)
import Servant
import Servant.Server.Internal.Delayed (passToServer)
import UnliftIO hiding (Handler)

data RequestInfo
  = RequestInfo
      { pathSegments :: [T.Text],
        capturedPathValues :: [(T.Text, T.Text)],
        queryParameters :: [(T.Text, T.Text)]
      }
  deriving (Eq, Show, Generic)

data TraceHandlerData = TraceHandlerData Wai.Request (Maybe RequestInfo)

data Honeycomb deriving (Typeable)

instance (HasRequestInfo api, HasServer api context) => HasServer (Honeycomb :> api) context where
  type ServerT (Honeycomb :> api) m = TraceHandlerData -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver (\req -> TraceHandlerData req $ getRequestInfo (Proxy :: Proxy api) req))
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

hoistHoneycombServer ::
  (MonadIO n, MonadReader env n, HC.HasSpanContext env, HasServer api '[]) =>
  Proxy api ->
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT (Honeycomb :> api) n
hoistHoneycombServer proxy =
  hoistHoneycombServerWithContext proxy (Proxy :: Proxy '[])

hoistHoneycombServerWithContext ::
  forall m n env api context.
  ( MonadIO n,
    MonadReader env n,
    HC.HasSpanContext env,
    HasServer api context
  ) =>
  Proxy api ->
  Proxy context ->
  (forall x. m x -> n x) ->
  ServerT api m ->
  TraceHandlerData ->
  ServerT api n
hoistHoneycombServerWithContext proxy proxycxt f server thd =
  hoistServerWithContext proxy proxycxt (updateTraceData thd . f) server
  where
    updateTraceData :: TraceHandlerData -> n x -> n x
    updateTraceData (TraceHandlerData req _) inner = do
      let spanContextRef = V.lookup spanContextKey (Wai.vault req)
      spanContext <- maybe (pure Nothing) readIORef spanContextRef
      local (over HC.spanContextL (const spanContext)) inner

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
