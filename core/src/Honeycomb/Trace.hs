{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Trace
  ( -- * Library initialization

    --
    -- $libraryInitialization
    module Honeycomb.Core,

    -- * Creating spans
    withNewSpan,
    withNewSpan',
    withNewRootSpan,
    withNewRootSpan',

    -- * Adding fields
    addField,
    add,

    -- * Modifying child span behavior
    --

    -- $modifyingSpans
    withInheritableFields,

    -- * Datatypes

    -- ** Trace types
    module Honeycomb.Trace.Types,

    -- ** Core types
    module Honeycomb.Core.Types,
  )
where

import Control.Monad.Reader (MonadReader, ask, local)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Honeycomb (newEvent, send')
import qualified Honeycomb as HC (add, addField)
import Honeycomb.Core
import Honeycomb.Core.Types
import Honeycomb.Trace.Types
import Lens.Micro (over, (&), (^.), _Just)
import Lens.Micro.Mtl (preview, view)
import UnliftIO

-- $libraryInitialization
-- To initialize the library, a `MonadReader` environment should be initialized, with
-- the "HasHoney" typeclass defined to say how to access the Honeycomb library. In
-- addition, the "HasSpanContext" typeclass needs a defined instance; this provides
-- context on the current trace and span being executed.
--
-- For example (using the RIO monad):
--
-- > import qualified Honeycomb as HC
-- > import Lens.Micro
-- > import RIO
-- >
-- > main :: IO ()
-- > main = HC.withHoney $ \honey ->
-- >   let app = App
-- >     { -- include other app context/settings
-- >     , appHoney = honey
-- >     , appSpanContext = Nothing
-- >     }
-- >   in runRIO app run
-- >
-- > data App =
-- >   App
-- >     { -- include other app context/settings
-- >     , appHoney :: !HC.Honey
-- >     , appSpanContext :: !(Maybe HC.SpanContext)
-- >     }
-- >
-- > instance HC.HasHoney App where
-- >     HC.honeyL = lens appHoney (\x y -> x { appHoney = y })
-- >
-- > instance HC.HasSpanContext App where
-- >     HC.spanContextL = lens appSpanContext (\x y -> x { appSpanContext = y })
--
-- It is less obvious how to give a general example for creating
-- spans, because it depends on the kind of code you are integrating
-- with.
--
-- For a HTTP service, you will usually put a 'withNewRootSpan'
-- or 'withNewRootSpan'' call in a place where it will be invoked on
-- every call into the service, and will encompass the vast majority
-- of time spent working on the call.

finishTrace ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  (Either SomeException a -> HoneyObject) ->
  m a ->
  m a
finishTrace f inner = do
  result <- try inner
  end <- liftIO getCurrentTime
  ctx <- fromJust <$> view spanContextL -- always called after setting tracecontext
  let event = spanEvent ctx
      start = event ^. eventTimestampL
      duration = toHoneyValue $ diffUTCTime end start
      extraFields =
        HoneyObject
          ( HM.fromList
              ( catMaybes
                  [ Just ("duration_ms", toHoneyValue duration),
                    Just ("service_name", toHoneyValue $ ctx ^. serviceNameL),
                    Just ("trace.trace_id", toHoneyValue $ getTraceId ctx),
                    Just ("trace.span_id", toHoneyValue $ getSpanId ctx),
                    (\e -> ("trace.parent_id", toHoneyValue e)) <$> parentSpanId ctx,
                    Just ("name", toHoneyValue $ spanName ctx)
                  ]
              )
          )
          <> f result
  send' extraFields event >> fromEither result

localTrace ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  SpanContext ->
  (Either SomeException a -> HoneyObject) ->
  m a ->
  m a
localTrace context f inner =
  finishTrace f inner & local (over spanContextL (const $ Just context))

-- | Starts a new root span (reporting on results or error).
--
-- This runs the supplied program in a new root span (i.e. no parent span wihin the current
-- process). If an existing span exists, it will be ignored. This call may cause a new trace to
-- be created, or it may continue an existing trace from a different service.
--
-- There is also a function which can cause additional fields to be added to the reported event,
-- which depends on the exception or result of the call.
withNewRootSpan ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  -- | The name of the service
  ServiceName ->
  -- | The name of the span
  SpanName ->
  -- | Parent span from an external system (if known)
  Maybe SpanReference ->
  -- | Additional fields to add based on result of program
  (Either SomeException a -> HoneyObject) ->
  -- | Program to run in the new span
  m a ->
  m a
withNewRootSpan serviceName spanName parentSpanRef f inner =
  case parentSpanRef of
    Just parentSpan -> do
      newContext <- createChildSpanContext parentSpan serviceName spanName HS.empty mempty
      localTrace newContext f inner
    Nothing -> do
      newContext <- createRootSpanContext serviceName spanName
      localTrace newContext f inner

-- | Starts a new root span (without reporting on results or error).
--
-- This runs the supplied program in a new root span (i.e. no parent span wihin the current
-- process). If an existing span exists, it will be ignored. This call may cause a new trace to
-- be created, or it may continue an existing trace from a different service.
withNewRootSpan' ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  -- | The name of the service
  ServiceName ->
  -- | The name of the span
  SpanName ->
  -- | Parent span from an external system (if known)
  Maybe SpanReference ->
  -- | Program to run in the new span
  m a ->
  m a
withNewRootSpan' serviceName spanName spanRef =
  withNewRootSpan serviceName spanName spanRef (const mempty)

-- | Starts a new child span (reporting on results or error).
--
-- This runs the supplied program in a new child span.
-- If an existing span exists, that will be marked as
-- the new span's parent. If there is no existing span, the
-- program is unchanged (i.e creating a trace must be
-- explicit).
--
-- There is also a function which can cause additional fields to be added to the reported event,
-- which depends on the exception or result of the call.
withNewSpan ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  -- | The name of the span
  SpanName ->
  -- | Additional fields to add based on result of program
  (Either SomeException a -> HoneyObject) ->
  -- | Program to run in the new span
  m a ->
  m a
withNewSpan spanName f inner = do
  oldEnv <- ask
  case oldEnv ^. spanContextL of
    Nothing -> inner
    Just oldCtx -> do
      let inheritableFields = oldCtx ^. inheritableFieldsL
          spanReference = oldCtx ^. spanReferenceL
          serviceName = oldCtx ^. serviceNameL
      inheritedFields <-
        if HS.null inheritableFields
          then pure mempty
          else
            HoneyObject
              <$> ( HM.filterWithKey (\k _ -> HS.member k inheritableFields)
                      <$> (coerce <$> readTVarIO (oldCtx ^. spanEventL . eventFieldsL))
                  )
      newContext <- createChildSpanContext spanReference serviceName spanName inheritableFields inheritedFields
      localTrace newContext f inner

-- | Starts a new child span (without reporting on results or error).
--
-- This runs the supplied program in a new child span.
-- If an existing span exists, that will be marked as
-- the new span's parent. If there is no existing span, the
-- program is unchanged (i.e creating a trace must be
-- explicit).
withNewSpan' ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env,
    HasSpanContext env
  ) =>
  -- | The name of the span
  SpanName ->
  -- | Program to run in the new span
  m a ->
  m a
withNewSpan' spanName =
  withNewSpan spanName (const mempty)

addField ::
  ( MonadIO m,
    ToHoneyValue v,
    MonadReader env m,
    HasSpanContext env
  ) =>
  T.Text ->
  v ->
  m ()
addField k v = do
  event <- preview (spanContextL . _Just . spanEventL)
  maybe (pure ()) (HC.addField k v) event

add ::
  ( MonadIO m,
    ToHoneyObject o,
    MonadReader env m,
    HasSpanContext env
  ) =>
  o ->
  m ()
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

-- | Modifies the list of fields which are inherited by child spans.
--
-- When a child span is created, it can copy the values of some fields from
-- the parent span. For example, you might want to include a user ID, or
-- similar value, to provide downstream context.
--
-- As the fields in a span are mutable, the values are copied at the time
-- that the child span is created; it will not inherit any values which
-- are added to the parent at a later point.
withInheritableFields ::
  ( MonadReader env m,
    HasSpanContext env
  ) =>
  -- | The modification to apply to the set of inheritable fields
  (HS.HashSet T.Text -> HS.HashSet T.Text) ->
  -- | The program to run with the modified set of fields
  m a ->
  m a
withInheritableFields modify inner =
  inner & local (over (spanContextL . _Just . inheritableFieldsL) modify)

createChildSpanContext ::
  ( MonadIO m,
    MonadReader env m,
    HasHoney env
  ) =>
  SpanReference ->
  ServiceName ->
  SpanName ->
  HS.HashSet T.Text ->
  HoneyObject ->
  m SpanContext
createChildSpanContext parentSpanRef serviceName spanName inheritableFields inheritedFields = do
  spanEvent <- newEvent
  HC.add inheritedFields spanEvent
  spanId <- mkSpanId
  pure
    SpanContext
      { spanReference = SpanReference (getTraceId parentSpanRef) spanId,
        parentSpanId = Just $ getSpanId parentSpanRef,
        serviceName = serviceName,
        spanName,
        spanEvent,
        inheritableFields
      }

createRootSpanContext ::
  ( MonadIO m,
    MonadReader env m,
    HasHoney env
  ) =>
  ServiceName ->
  SpanName ->
  m SpanContext
createRootSpanContext serviceName spanName = do
  spanEvent <- newEvent
  threadId <- mkTraceId
  spanId <- mkSpanId
  pure
    SpanContext
      { spanReference = SpanReference threadId spanId,
        parentSpanId = Nothing,
        serviceName,
        spanName,
        spanEvent,
        inheritableFields = HS.empty
      }
