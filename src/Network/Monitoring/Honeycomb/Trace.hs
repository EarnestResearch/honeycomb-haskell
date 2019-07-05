{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.Monitoring.Honeycomb.Trace
    (
    -- * Library initialization
    --
    -- $libraryInitialization

    -- * Creating spans

      withNewRootSpan
    , withNewSpan
    , withNewSpanFromHeaders
    
    -- * Adding fields
    , addTraceField
    , addTraceFieldSTM
    , addTraceFields
    , addTraceFieldsSTM
    , module Network.Monitoring.Honeycomb.Trace.Types
    )
where

import Lens.Micro ((^?), _Just)
import Lens.Micro.Mtl (preview)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Monitoring.Honeycomb.Trace.Types
import RIO
import RIO.Partial (fromJust)
import RIO.Time

import qualified Network.Monitoring.Honeycomb as HC
import qualified RIO.HashMap as HM

-- $libraryInitialization
--
-- The tracing library is set up by creating a @Tracer@ value,
-- placing it in a @MonadReader@ environment, and defining the
-- @HasTracer@ typeclass for the environment.
--
-- It also expects the base Honeycomb library to be configured
-- and available. For example:
--
-- > do
-- >     let ho = defaultHoneyOptions
-- >           & apiKeyL ?~ "12345678"
-- >           & datasetL ?~ "test-dataset"
-- >         appTracer = mkTracer "test_service"
-- >     withHoney defaultHoneyServerOptions ho mempty $ \appHoney ->
-- >         let app = App
-- >               { -- include other app context/settings
-- >               , appHoney
-- >               , appTracer
-- >               }
-- >         in runRIO app run
--
-- > instance HasHoney App where
-- >     honeyL = lens appHoney (\x y -> x { appHoney = y })
-- >
-- > instance HasTracer App where
-- >     tracerL = lens appTracer (\x y -> x { appTracer = y })

finishTrace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HC.HasHoney env
       , HasTracer env
       )
    => (Either SomeException a -> HC.HoneyObject)
    -> m a
    -> m a
finishTrace f inner = do
    result <- try inner
    end <- getCurrentTime
    tracer <- view tracerL
    let 
        ctx = fromJust $ tracer ^. spanContextL  -- always called after setting tracecontext
        event = spanEvent ctx
        start = event ^. HC.eventTimestampL
        duration = HC.toHoneyValue $ diffUTCTime end start
        extraFields = HM.fromList (catMaybes
          [ Just ("duration_ms", HC.toHoneyValue duration)
          , Just ("service_name", HC.toHoneyValue $ tracer ^. serviceNameL)
          , Just ("trace.trace_id", HC.toHoneyValue $ getTraceId ctx)
          , Just ("trace.span_id", HC.toHoneyValue $ getSpanId ctx)
          , (\e -> ("trace.parent_id", HC.toHoneyValue e)) <$> parentSpanId ctx
          , Just ("name", HC.toHoneyValue $ spanName ctx)
          ]) `HM.union` f result
    HC.send' extraFields event >> fromEither result

locally :: MonadReader s m => ASetter s s a b -> (a -> b) -> m r -> m r
locally l f = local (over l f)
{-# INLINE locally #-}
    
localTrace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HC.HasHoney env
       , HasTracer env
       )
    => SpanContext
    -> (Either SomeException a -> HC.HoneyObject)
    -> m a
    -> m a 
localTrace context f inner =
    finishTrace f inner & locally spanContextL (const $ Just context)

withNewRootSpan
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HC.HasHoney env
       , HasTracer env)
    => SpanName
    -> (Either SomeException a -> HC.HoneyObject)
    -> m a
    -> m a
withNewRootSpan spanName f inner = do
    newContext <- createRootSpanContext spanName
    localTrace newContext f inner

withNewSpan
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HC.HasHoney env
       , HasTracer env
       )
    => SpanName
    -> (Either SomeException a -> HC.HoneyObject)
    -> m a
    -> m a
withNewSpan spanName f inner = do
    oldEnv <- ask
    let oldRef = oldEnv ^? spanContextL . _Just . spanReferenceL
    newContext <- createRootOrChildSpanContext oldRef spanName
    localTrace newContext f inner

withNewSpanFromHeaders
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HC.HasHoney env
       , HasTracer env
       )
    => SpanName
    -> RequestHeaders
    -> (Either SomeException a -> HC.HoneyObject)
    -> m a
    -> m a
withNewSpanFromHeaders spanName headers f inner = do
    oldEnv <- ask
    let toPropagationData = oldEnv ^. tracerL . propagationL . toPropagationDataL
        spanRef = view propagationSpanReferenceL <$> toPropagationData headers
    newContext <- createRootOrChildSpanContext spanRef spanName 
    localTrace newContext f inner

addTraceFieldSTM
    :: ( HC.ToHoneyValue v
       , MonadReader env m
       , HasTracer env
       )
    => Text
    -> v
    -> m (STM ())
addTraceFieldSTM k v =
    maybe (pure ()) (HC.addFieldSTM k v) <$> preview (spanContextL . _Just . spanEventL)

addTraceField
    :: ( MonadIO m
       , HC.ToHoneyValue v
       , MonadReader env m
       , HasTracer env
       )
    => Text
    -> v
    -> m ()
addTraceField k v =
    addTraceFieldSTM k v >>= atomically

addTraceFieldsSTM
    :: ( HC.ToHoneyObject o
       , MonadReader env m
       , HasTracer env
       )
    => o
    -> m (STM ())
addTraceFieldsSTM fields =
    maybe (pure ()) (HC.addFieldsSTM fields) <$> preview (spanContextL . _Just . spanEventL)

addTraceFields
    :: ( MonadIO m
       , HC.ToHoneyObject o
       , MonadReader env m
       , HasTracer env
       )
    => o
    -> m ()
addTraceFields fields =
    addTraceFieldsSTM fields >>= atomically

createRootOrChildSpanContext
    :: ( MonadIO m
       , MonadReader env m
       , HC.HasHoney env
       )
    => Maybe SpanReference
    -> SpanName
    -> m SpanContext
createRootOrChildSpanContext (Just ref) spanName = createChildSpanContext spanName ref
createRootOrChildSpanContext Nothing spanName = createRootSpanContext spanName

createChildSpanContext
    :: ( MonadIO m
       , MonadReader env m
       , HC.HasHoney env
       )
    => SpanName
    -> SpanReference
    -> m SpanContext
createChildSpanContext spanName parentSpanRef = do
    spanEvent <- HC.newEvent
    spanId <- mkSpanId
    pure SpanContext
        { spanReference = mkSpanReference (getTraceId parentSpanRef) spanId
        , parentSpanId = Just $ getSpanId parentSpanRef
        , spanName
        , spanEvent
        }

createRootSpanContext
    :: ( MonadIO m
       , MonadReader env m
       , HC.HasHoney env
       )
    => SpanName
    -> m SpanContext
createRootSpanContext spanName = do
    spanEvent <- HC.newEvent
    threadId <- mkTraceId
    spanId <- mkSpanId
    pure SpanContext
        { spanReference = mkSpanReference threadId spanId
        , parentSpanId = Nothing
        , spanName
        , spanEvent
        }
