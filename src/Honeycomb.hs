{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Honeycomb
-- Description : Client library for @honeycomb.io@
-- Copyright   : (c) 2019-2020 Earnest Research
-- License     : Apache-2
-- Maintainer  : gcoady@earnestresearch.com
-- Stability   : alpha
-- Portability : POSIX
--
-- The "Honeycomb" module provides an interface to allow users to
-- initialize the library, and manually create and submit events to be
-- sent to the service.
--
-- If you are interested in a tracing API, which handles timing of blocks
-- of code, along with the creation of /traces/ and /spans/, you should
-- look at the "Honeycomb.Trace" module.
-- If you want a very low-level interface for submitting data to the HTTP
-- interface of the Honeycomb service, then you should look at the
-- "Honeycomb.Api" module.
module Honeycomb
  ( -- * Library initialization
    --
    -- $libraryInitialization
    module Honeycomb.Core,

    -- * Events

    -- ** Creating and sending
    newEvent,
    newEventWithMetadata,
    send,
    send',

    -- ** Adding fields
    --
    -- $addingFields
    addField,
    add,

    -- * Datatypes

    -- ** Core types
    module Honeycomb.Core.Types,
  )
where

import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Dynamic (toDyn)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Honeycomb.Api as Api
import Honeycomb.Core
import Honeycomb.Core.Internal.Types
import Honeycomb.Core.Types
import Lens.Micro
import Lens.Micro.Mtl (view)
import UnliftIO

-- $libraryInitialization
-- To initialize the library, a `MonadReader` environment should be initialized, with
-- the `HasHoney` typeclass defined to say how to access the Honeycomb library.
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
-- >     }
-- >   in runRIO app run
-- >
-- > data App =
-- >   App
-- >     { -- include other app context/settings
-- >     , appHoney :: !HC.Honey
-- >     }
-- >
-- > instance HC.HasHoney App where
-- >     HC.honeyL = lens appHoney (\x y -> x { appHoney = y })

-- | Creates a new Honeycomb event with no extra fields added.
newEvent ::
  ( MonadIO m,
    MonadReader env m,
    HasHoney env
  ) =>
  m HoneyEvent
newEvent = view (honeyL . honeyOptionsL) >>= mkHoneyEvent

newEventWithMetadata ::
  forall m env a.
  ( MonadIO m,
    MonadReader env m,
    HasHoney env,
    Typeable a
  ) =>
  a ->
  m HoneyEvent
newEventWithMetadata metadata =
  (eventMetadataL ?~ toDyn metadata) <$> newEvent

-- $addingFields
--
-- The 'HoneyEvent' allows fields to be added at any point between
-- when the event was created, and when the event is sent.
--
-- In order to support field addition across threads, the list of
-- events is stored in a mutable variable. A user of the library
-- may add fields from any thread.
--
-- When the event is queued for sending, a snapshot of the fields is
-- taken at that point. Further changes to the mutable variable from
-- that (or any other thread) will not be reflected in the event.

-- | Adds a field to the event.
--
-- This allows fields to be added to an event between its creation and
-- sending. Because it uses mutable variables, fields can be added
-- from different threads, and will be visible as long as the mutation
-- occurs before the event is sent.
addField ::
  ( MonadIO m,
    ToHoneyValue v
  ) =>
  -- | The name of the field
  T.Text ->
  -- | The value of the field
  v ->
  -- | The event to which the field will be added
  HoneyEvent ->
  m ()
addField k v evt =
  atomically $ modifyTVar' (evt ^. eventFieldsL) $ \e -> coerce $ HM.insert k (toHoneyValue v) $ coerce e

-- | Adds multiple fields to the event.
--
-- This allows fields to be added to an event between its creation and
-- sending. Because it uses mutable variables, fields can be added
-- from different threads, and will be visible as long as the mutation
-- occurs before the event is sent.
add ::
  ( MonadIO m,
    ToHoneyObject o
  ) =>
  -- | The fields to be added
  o ->
  -- | The event to which the fields will be added
  HoneyEvent ->
  m ()
add fields evt =
  atomically $ modifyTVar' (evt ^. eventFieldsL) (toHoneyObject fields <>)

-- | Queues the event for sending to the Honeycomb service.
--
-- This takes a snapshot of the current fields, and queues the event for
-- sending. Once the event is queued, the request returns.
--
-- The request will not block unless the event queue is full, and the
-- library has been configured to block instead of dropping events.
send ::
  ( MonadIO m,
    MonadReader env m,
    HasHoney env
  ) =>
  -- | The event to be sent
  HoneyEvent ->
  m ()
send = send' (mempty :: HoneyObject)

-- | Queues the event for sending to the Honeycomb service.
--
-- This takes a snapshot of the current fields, and queues the event for
-- sending. Once the event is queued, the request returns.
--
-- The request will not block unless the event queue is full, and the
-- library has been configured to block instead of dropping events.
--
-- This variant also takes a set of additional fields to be added before
-- sending.
send' ::
  forall m env o.
  ( MonadIO m,
    MonadReader env m,
    ToHoneyObject o,
    HasHoney env
  ) =>
  -- | An additional set of fields to add before sending
  o ->
  -- | The event to be sent
  HoneyEvent ->
  m ()
send' extraFields event = do
  let shouldSample = True
  apiEvent <- toApiEvent
  requestOpts <- toRequestOptions
  case requestOpts of
    Nothing -> pure ()
    Just opts -> sendApiEvent shouldSample opts apiEvent
  where
    toRequestOptions :: m (Maybe Api.RequestOptions)
    toRequestOptions =
      if event ^. eventOptionsL . disabledL
        then pure Nothing
        else do
          ds <- maybe (throwIO MissingDatasetOption) pure $ event ^. eventOptionsL . datasetL
          key <- maybe (throwIO MissingApiKeyOption) pure $ event ^. eventOptionsL . apiKeyL
          pure $ Just $ Api.mkRequestOptions (event ^. eventOptionsL . apiHostL) ds key
    toApiEvent :: m Api.Event
    toApiEvent = do
      eventFields <- readTVarIO (event ^. eventFieldsL)
      let finalFields = toHoneyObject extraFields <> eventFields
      pure $
        Api.mkEvent finalFields
          & Api.eventTimestampL ?~ event ^. eventTimestampL
          & Api.eventSampleRateL ?~ event ^. eventOptionsL . sampleRateL
          & Api.eventMetadataL .~ event ^. eventMetadataL
    sendApiEvent ::
      Bool ->
      RequestOptions ->
      Api.Event ->
      m ()
    sendApiEvent shouldSample requestOpts apiEvent = do
      if HM.null (coerce (apiEvent ^. Api.eventFieldsL) :: HM.HashMap T.Text HoneyValue)
        then throwIO EmptyEventData
        else pure ()
      ctx <- view honeyL
      let sendQ = ctx ^. honeyTransportStateL . transportSendQueueL
          opts = ctx ^. honeyOptionsL
          shouldBlock = opts ^. blockOnSendL
          responseQueue = ctx ^. honeyTransportStateL . transportResponseQueueL
      atomically $
        case (shouldSample, shouldBlock) of
          (True, True) ->
            writeTBQueue sendQ (requestOpts, apiEvent)
          (True, False) ->
            do
              full <- isFullTBQueue sendQ
              if full
                then writeTBQueue responseQueue $ HoneyResponse
                  { honeyResponseMetadata = apiEvent ^. Api.eventMetadataL,
                    honeyResponseStatusCode = Nothing,
                    honeyResponseError = Just "Event dropped due to queue overflow"
                  }
                else writeTBQueue sendQ (requestOpts, apiEvent)
          (False, _) ->
            writeTBQueue responseQueue $ HoneyResponse
              { honeyResponseMetadata = apiEvent ^. Api.eventMetadataL,
                honeyResponseStatusCode = Nothing,
                honeyResponseError = Just "Event dropped due to client-side sampling"
              }
