{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Honeycomb
Description : Client library for @honeycomb.io@
Copyright   : (c) Gary Coady, 2019
License     : Apache-2
Maintainer  : gcoady@earnestresearch.com
Stability   : experimental
Portability : POSIX

Honeycomb is a SAAS monitoring system based around reporting of
events, with support for high-cardinality attributes.
-}
module Honeycomb
    (
    -- * Events

    -- ** Creating and sending
      newEvent
    , send
    , send'

    -- ** Adding fields
    --
    -- $addingFields
    , addField
    , add

    , module Honeycomb.Core
    )
where

import Control.Monad.Reader (MonadReader)
import Honeycomb.Core
import Lens.Micro (to, (^.))
import Lens.Micro.Mtl (view)
import UnliftIO

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Honeycomb.Api as Api

-- $libraryInitialization
-- To initialize the library, a `MonadReader` environment should be initialized, with
-- the `HasHoney` typeclass defined to say how to access the Honeycomb library.
--
-- For example:
--
-- > do
-- >     let ho = defaultHoneyOptions
-- >           & apiKeyL ?~ "12345678"
-- >           & datasetL ?~ "test-dataset"
-- >     withHoney defaultHoneyServerOptions ho $ \appHoney ->
-- >         let app = App
-- >               { -- include other app context/settings
-- >               , appHoney
-- >               }
-- >         in runRIO app run
--
-- > instance HasHoney App where
-- >     honeyL = lens appHoney (\x y -> x { appHoney = y })

-- | Creates a new Honeycomb event with no extra fields added.
newEvent
    :: ( MonadIO m
       , MonadReader env m
       , HasHoney env
       )
    => m HoneyEvent
newEvent = view (honeyL . honeyOptionsL) >>= mkHoneyEvent
 
-- $addingFields
--
-- The @HoneyEvent@ allows fields to be added at any point between
-- when the event was created, and when the event is sent.
--
-- In order to support field addition across threads, the list of
-- events is stored in a mutable variable. A user of the library
-- may add fields from any thread.
--
-- When the event is queued for sending, a snapshot of the fields is
-- taken at that point. Further changes to the mutable variable from
-- that (or any other thread) will not be reflected in the event.

{- | Adds a field to the event.

This allows fields to be added to an event between its creation and
sending. Because it uses mutable variables, fields can be added
from different threads, and will be visible as long as the mutation
occurs before the event is sent.
-}
addField
    :: ( MonadIO m
       , ToHoneyValue v
       )
    => T.Text      -- ^ The name of the field
    -> v           -- ^ The value of the field
    -> HoneyEvent  -- ^ The event to which the field will be added
    -> m ()
addField k v evt =
    atomically $ modifyTVar' (evt ^. eventFieldsL) $ HM.insert k (toHoneyValue v)

{- | Adds multiple fields to the event.

This allows fields to be added to an event between its creation and
sending. Because it uses mutable variables, fields can be added
from different threads, and will be visible as long as the mutation
occurs before the event is sent.
-}
add
    :: ( MonadIO m
       , ToHoneyObject o
       )
    => o           -- ^ The fields to be added
    -> HoneyEvent  -- ^ The event to which the fields will be added
    -> m ()
add fields evt =
    atomically $ modifyTVar' (evt ^. eventFieldsL) (toHoneyObject fields `HM.union`)

{- | Queues the event for sending to the Honeycomb service.

This takes a snapshot of the current fields, and queues the event for
sending. Once the event is queued, the request returns.

The request will not block unless the event queue is full, and the
library has been configured to block instead of dropping events.
-}
send
    :: ( MonadIO m
       , MonadReader env m
       , HasHoney env
       )
    => HoneyEvent  -- ^ The event to be sent
    -> m ()
send = send' (HM.empty :: HoneyObject)

{- | Queues the event for sending to the Honeycomb service.

This takes a snapshot of the current fields, and queues the event for
sending. Once the event is queued, the request returns.

The request will not block unless the event queue is full, and the
library has been configured to block instead of dropping events.

This variant also takes a set of additional fields to be added before
sending. 
-}
send'
    :: forall m env o .
       ( MonadIO m
       , MonadReader env m
       , ToHoneyObject o
       , HasHoney env
       )
    => o           -- ^ An additional set of fields to add before sending
    -> HoneyEvent  -- ^ The event to be sent
    -> m ()
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
        if event ^. eventOptionsL . disabledL then
            pure Nothing
        else do
            ds <- maybe (throwIO MissingDatasetOption) pure $ event ^. eventOptionsL . datasetL
            key <- maybe (throwIO MissingApiKeyOption) pure $ event ^. eventOptionsL . apiKeyL
            pure $ Just $ Api.mkRequestOptions (event ^. eventOptionsL . apiHostL) ds key

    toApiEvent :: m Api.Event
    toApiEvent = do
        eventFields <- readTVarIO (event ^. eventFieldsL)
        let finalFields = HM.union (toHoneyObject extraFields) eventFields
        pure $ Api.mkEvent
            finalFields
            (event ^. eventTimestampL . to Just)
            (event ^. eventOptionsL . sampleRateL . to Just)

    sendApiEvent :: Bool -> RequestOptions -> Api.Event -> m ()
    sendApiEvent shouldSample requestOpts apiEvent = do
        if List.null $ apiEvent ^. Api.eventFieldsL then
            throwIO EmptyEventData
        else
            pure ()
        ctx <- view honeyL
        let sendQ = ctx ^. honeyTransportStateL . transportSendQueueL
            opts = ctx ^. honeyOptionsL
            shouldBlock = opts ^. blockOnSendL
        if shouldSample then atomically $
            if shouldBlock then
                writeTBQueue sendQ (requestOpts, apiEvent)
            else do
                full <- isFullTBQueue sendQ
                if full then
                    pure ()  -- [todo] add reporting of queue overflow
                else
                    writeTBQueue sendQ (requestOpts, apiEvent)
        else
            pure ()