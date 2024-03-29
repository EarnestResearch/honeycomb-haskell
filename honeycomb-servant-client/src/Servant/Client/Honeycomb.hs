{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Client.Honeycomb
  ( TraceClientM,
    HasClientEnv,
    clientEnvL,
    runTraceClientM,
    traceClient,
    unTraceClientM,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Functor.Alt (Alt (..))
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Honeycomb.Trace as HC
import Lens.Micro (Lens', (^.))
import Lens.Micro.Mtl (view)
import Servant.Client.Core (Client, HasClient, Request, RunClient (..), addHeader, clientIn)
import Servant.Client.Internal.HttpClient
import UnliftIO
import qualified Data.List as List
import Network.HTTP.Types.Header (HeaderName)

class HasClientEnv env where
  clientEnvL :: Lens' env ClientEnv

newtype TraceClientM env a = TraceClientM {unTraceClientM :: ReaderT env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadThrow)

instance MonadUnliftIO (TraceClientM env) where
  withRunInIO inner = TraceClientM $ withRunInIO $ \run -> inner (run . unTraceClientM)
  {-# INLINE withRunInIO #-}

instance Alt (TraceClientM env) where
  a <!> b = a `catchIO` const b

instance (HasClientEnv env, HC.HasSpanContext env) => RunClient (TraceClientM env) where
  runRequestAcceptStatus ss req =
    do
      spanContext <- view HC.spanContextL
      clientEnv <- view clientEnvL
      let reqWithTraceHeader = traceValue spanContext req
      nt clientEnv $ performRequest ss reqWithTraceHeader
    where
      nt :: ClientEnv -> ClientM response -> TraceClientM env response
      nt clientEnv (ClientM response) =
        TraceClientM .
          liftIO $
            runExceptT (runReaderT response clientEnv) >>= either throwIO pure
      traceValue :: Maybe HC.SpanContext -> Request -> Request
      traceValue Nothing _ = req
      traceValue (Just sc) r =
        List.foldl' (flip ($)) r (uncurry addHeader <$> getHeaders (sc ^. HC.spanReferenceL))
      getHeaders :: HC.SpanReference -> [(HeaderName, T.Text)]
      getHeaders = HC.writeTraceHeader HC.honeycombTraceHeaderFormat

  throwClientError = throwIO

traceClient :: forall api env. HasClient (TraceClientM env) api => Proxy api -> Proxy env -> Client (TraceClientM env) api
traceClient api _ = api `clientIn` (Proxy :: Proxy (TraceClientM env))

runTraceClientM :: TraceClientM env a -> env -> IO a
runTraceClientM tcm env = flip runReaderT env $ unTraceClientM tcm
