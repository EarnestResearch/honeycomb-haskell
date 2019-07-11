module Honeycomb.Trace
    (
    -- * Library initialization
    --
    -- $libraryInitialization

    -- * Creating spans

      withNewSpan
    , withNewSpan'
    , withNewRootSpan
    , withNewRootSpan'
    
    -- * Adding fields

    , addField
    , add

    -- * Modifying child span behavior
    --
    -- $modifyingSpans

    , withInheritableFields

    -- * Types

    , module Honeycomb.Trace.Types

    -- * Imported from Honeycomb

    , module Honeycomb
    )
where

import Lens.Micro (_Just)
import Lens.Micro.Mtl (preview)
import Honeycomb (HasHoney, HoneyObject, toHoneyValue, HoneyValue (..))
import Honeycomb.Trace.Types
import RIO
import RIO.Partial (fromJust)
import RIO.Time

import qualified Honeycomb as HC (ToHoneyValue, ToHoneyObject, add, addField, eventTimestampL, eventFieldsL, newEvent, send')
import qualified RIO.HashMap as HM
import qualified RIO.Set as Set

-- $libraryInitialization
--
-- The tracing library is set up by creating @HasSpanContext@
-- typeclass instance for a @MonadReader@ environment.
-- This should point to a value of @Maybe SpanContext@ in the
-- environment; this can be initialized to @Nothing@ at
-- startup.
--
-- It also expects the base Honeycomb library to be configured
-- and available. For example:
--
-- > do
-- >     let ho = defaultHoneyOptions
-- >           & apiKeyL ?~ "12345678"
-- >           & datasetL ?~ "test-dataset"
-- >     withHoney defaultHoneyServerOptions ho mempty $ \appHoney ->
-- >         let app = App
-- >               { -- include other app context/settings
-- >               , appHoney
-- >               , appSpanContext = Nothing
-- >               }
-- >         in runRIO app run
--
-- > instance HasHoney App where
-- >     honeyL = lens appHoney (\x y -> x { appHoney = y })
-- >
-- > instance HasSpanContext App where
-- >     spanContextL = lens spanContextL (\x y -> x { spanContextL = y })

finishTrace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env
       )
    => (Either SomeException a -> HoneyObject)
    -> m a
    -> m a
finishTrace f inner = do
    result <- try inner
    end <- getCurrentTime
    ctx <- fromJust <$> view spanContextL  -- always called after setting tracecontext
    let 
        event = spanEvent ctx
        start = event ^. HC.eventTimestampL
        duration = toHoneyValue $ diffUTCTime end start
        extraFields = HM.fromList (catMaybes
          [ Just ("duration_ms", toHoneyValue duration)
          , Just ("service_name", toHoneyValue $ ctx ^. serviceNameL)
          , Just ("trace.trace_id", toHoneyValue $ getTraceId ctx)
          , Just ("trace.span_id", toHoneyValue $ getSpanId ctx)
          , (\e -> ("trace.parent_id", toHoneyValue e)) <$> parentSpanId ctx
          , Just ("name", toHoneyValue $ spanName ctx)
          ]) `HM.union` f result
    HC.send' extraFields event >> fromEither result
    
localTrace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env
       )
    => SpanContext
    -> (Either SomeException a -> HoneyObject)
    -> m a
    -> m a 
localTrace context f inner =
    finishTrace f inner & local (over spanContextL (const $ Just context))

{- | Starts a new root span.

This runs the supplied program in a new root span (i.e. no parent span). If an existing
span exists, it will be ignored.
-}
withNewRootSpan
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env)
    => ServiceName                                 -- ^ The name of the service
    -> SpanName                                    -- ^ The name of the span
    -> Maybe SpanReference                         -- ^ Parent span from an external system (if known)
    -> (Either SomeException a -> HoneyObject)     -- ^ Additional fields to add based on result of program
    -> m a                                         -- ^ Program to run in the new span
    -> m a
withNewRootSpan serviceName spanName parentSpanRef f inner =
    case parentSpanRef of
        Just parentSpan -> do
            newContext <- createChildSpanContext parentSpan serviceName spanName Set.empty HM.empty
            localTrace newContext f inner
        Nothing -> do
            newContext <- createRootSpanContext serviceName spanName
            localTrace newContext f inner

withNewRootSpan'
    :: ( MonadUnliftIO m
        , MonadReader env m
        , HasHoney env
        , HasSpanContext env)
    => ServiceName                                 -- ^ The name of the service
    -> SpanName                                    -- ^ The name of the span
    -> Maybe SpanReference                         -- ^ Parent span from an external system (if known)
    -> m a                                         -- ^ Program to run in the new span
    -> m a
withNewRootSpan' serviceName spanName spanRef =
    withNewRootSpan serviceName spanName spanRef (const mempty)

{- | Starts a new child span.

This runs the supplied program in a new child span.
If an existing span exists, that will be marked as
the new span's parent. If there is no existing span, the
program is unchanged (i.e creating a trace must be
explicit).
-}
withNewSpan
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       , HasSpanContext env
       )
    => SpanName                                    -- ^ The name of the span
    -> (Either SomeException a -> HoneyObject)     -- ^ Additional fields to add based on result of program
    -> m a                                         -- ^ Program to run in the new span
    -> m a
withNewSpan spanName f inner = do
    oldEnv <- ask
    case oldEnv ^. spanContextL of
        Nothing -> inner
        Just oldCtx -> do
            let inheritableFields = oldCtx ^. inheritableFieldsL
                spanReference = oldCtx ^. spanReferenceL
                serviceName = oldCtx ^. serviceNameL
            inheritedFields <-
                if Set.null inheritableFields then
                    pure $ HM.empty
                else
                    HM.filterWithKey (\k _ -> Set.member k inheritableFields)
                        <$> readTVarIO (oldCtx ^. spanEventL . HC.eventFieldsL)
            newContext <- createChildSpanContext spanReference serviceName spanName inheritableFields inheritedFields 
            localTrace newContext f inner

withNewSpan'
    :: ( MonadUnliftIO m
        , MonadReader env m
        , HasHoney env
        , HasSpanContext env
        )
    => SpanName                                    -- ^ The name of the span
    -> m a                                         -- ^ Program to run in the new span
    -> m a
withNewSpan' spanName =
    withNewSpan spanName (const mempty)

addField
    :: ( MonadIO m
       , HC.ToHoneyValue v
       , MonadReader env m
       , HasSpanContext env
       )
    => Text
    -> v
    -> m ()
addField k v = do
    event <- preview (spanContextL . _Just . spanEventL)
    maybe (pure ()) (HC.addField k v) event

add
    :: ( MonadIO m
       , HC.ToHoneyObject o
       , MonadReader env m
       , HasSpanContext env
       )
    => o
    -> m ()
add fields = do
    event <- preview (spanContextL . _Just . spanEventL)
    maybe (pure ()) (HC.add fields) event

-- $modifyingSpans
--
-- Spans are represented by a @SpanContext@ field, which is generally
-- immutable (other than the set of fields stored in the current event).
-- To modify behavior of child spans, we may modify the span context,
-- and provide that context in a modified environment for future calls.
--
-- This section provides helpers for modifying useful settings and/or
-- parameters.

{- | Modifies the list of fields which are inherited by child spans.

When a child span is created, it can copy the values of some fields from
the parent span. For example, you might want to include a user ID, or
similar value, to provide downstream context.

As the fields in a span are mutable, the values are copied at the time
that the child span is created; it will not inherit any values which
are added to the parent at a later point.
-}
withInheritableFields
    :: ( MonadReader env m
       , HasSpanContext env
       )
    => (Set Text -> Set Text)  -- ^ The modification to apply to the set of inheritable fields
    -> m a                     -- ^ The program to run with the modified set of fields
    -> m a
withInheritableFields modify inner = do
    inner & local (over (spanContextL . _Just . inheritableFieldsL) (modify))

createChildSpanContext
    :: ( MonadIO m
       , MonadReader env m
       , HasHoney env
       )
    => SpanReference
    -> ServiceName
    -> SpanName
    -> Set Text
    -> HoneyObject
    -> m SpanContext
createChildSpanContext parentSpanRef serviceName spanName inheritableFields inheritedFields = do
    spanEvent <- HC.newEvent
    HC.add inheritedFields spanEvent
    spanId <- mkSpanId
    pure SpanContext
        { spanReference = SpanReference (getTraceId parentSpanRef) spanId
        , parentSpanId = Just $ getSpanId parentSpanRef
        , serviceName = serviceName
        , spanName
        , spanEvent
        , inheritableFields
        }

createRootSpanContext
    :: ( MonadIO m
       , MonadReader env m
       , HasHoney env
       )
    => ServiceName
    -> SpanName
    -> m SpanContext
createRootSpanContext serviceName spanName = do
    spanEvent <- HC.newEvent
    threadId <- mkTraceId
    spanId <- mkSpanId
    pure SpanContext
        { spanReference = SpanReference threadId spanId
        , parentSpanId = Nothing
        , serviceName
        , spanName
        , spanEvent
        , inheritableFields = Set.empty
        }
