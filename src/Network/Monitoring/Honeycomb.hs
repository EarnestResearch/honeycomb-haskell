{-|
Module      : Network.Monitoring.Honeycomb
Description : Client library for @honeycomb.io@
Copyright   : (c) Gary Coady, 2019
License     : Apache-2
Maintainer  : gcoady@earnestresearch.com
Stability   : experimental
Portability : POSIX

Honeycomb is a SAAS monitoring system based around reporting of
events, with support for high-cardinality attributes.
-}
module Network.Monitoring.Honeycomb
    (
    -- * Library initialization
    --
    -- $libraryInitialization
      withHoney
    , newHoney

    -- * Events

    -- ** Creating and sending
    , newEvent
    , send
    , send'
    , flush

    -- ** Adding fields
    --
    -- $addingFields
    , addField
    , addFieldSTM
    , add
    , addSTM

    , module Network.Monitoring.Honeycomb.Types
    )
where

import Network.Monitoring.Honeycomb.Transport
import Network.Monitoring.Honeycomb.Types
import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
import Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
import RIO

import qualified RIO.HashMap as HM
import qualified RIO.List as List

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
-- >     withHoney defaultHoneyServerOptions ho mempty $ \appHoney ->
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
newEvent = do
    honey <- view honeyL
    mkHoneyEvent (honey ^. honeyOptionsL) (honey ^. defaultFieldsL)
 
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

{- | Adds a field to the event (in the STM monad).

This allows fields to be added to an event between its creation and
sending. Because it uses mutable variables, fields can be added
from different threads, and will be visible as long as the mutation
occurs before the event is sent.
-}
addFieldSTM
    :: ( ToHoneyValue v
       )
    => Text        -- ^ The name of the field
    -> v           -- ^ The value of the field
    -> HoneyEvent  -- ^ The event to which the field will be added
    -> STM ()
addFieldSTM k v evt =
    modifyTVar' (evt ^. eventFieldsL) $ HM.insert k (toHoneyValue v)

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
    => Text        -- ^ The name of the field
    -> v           -- ^ The value of the field
    -> HoneyEvent  -- ^ The event to which the field will be added
    -> m ()
addField k v =
    atomically . addFieldSTM k v

{- | Adds multiplee fields to the event (in the STM monad).

This allows fields to be added to an event between its creation and
sending. Because it uses mutable variables, fields can be added
from different threads, and will be visible as long as the mutation
occurs before the event is sent.
-}
addSTM
    :: ( ToHoneyObject o
       )
    => o           -- ^ The fields to be added
    -> HoneyEvent  -- ^ The event to which the fields will be added
    -> STM ()
addSTM fields evt =
    modifyTVar' (evt ^. eventFieldsL) (toHoneyObject fields `HM.union`)

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
add fields =
    atomically . addSTM fields

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
send' extraFields event =
    newFrozenEvent (toHoneyObject extraFields) event >>= sendFrozen
  where
    sendFrozen :: FrozenHoneyEvent -> m ()
    sendFrozen evt = do
        if List.null $ evt ^. frozenEventFieldsL then
            throwIO EmptyEventData
        else
            pure ()
        ctx <- view honeyL
        let sendQ = ctx ^. sendQueueL
            opts = ctx ^. honeyOptionsL
            shouldBlock = opts ^. blockOnSendL
            shouldSample = evt ^. frozenEventShouldSampleL
        if shouldSample then atomically $
            if shouldBlock then
                writeTBQueue sendQ $ QueueMessage evt
            else do
                full <- isFullTBQueue sendQ
                if full then
                    pure ()  -- [todo] add reporting of queue overflow
                else
                    writeTBQueue sendQ $ QueueMessage evt
        else
            pure ()

{- | Waits until all currently sent events have been dequeued and processed.

This may be useful in a system which suspends processing when idle; the user
may want to guarantee that all queued events have been sent.

This only guarantees that events queued before this call will be sent. A
user may add more events afterwards, and this does not guarantee that those
events have been sent.
-}
flush
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasHoney env
       )
    => Int  -- ^ Length of time to wait before giving up (in microseconds)
    -> m ()
flush timeout_us = do
    honey <- view honeyL
    mvar <- newEmptyMVar
    _ <- atomically $ writeTBQueue (honey ^. sendQueueL) $ FlushQueue mvar
    void $ timeout timeout_us $ takeMVar mvar

{- | Creates a new Honey library instance.

A background thread is started up, which will dequeue events that
need to be sent. On shutdown, the event queue is shut down, and
the background thread stops once all messages are processed. 
-}
newHoney
    :: ( MonadUnliftIO n
       , MonadIO m
       )
    => HoneyServerOptions  -- ^ Options for how event handling is performed
    -> HoneyOptions        -- ^ Options for client library behaviour
    -> HoneyObject         -- ^ Default fields added to all events
    -> n (Honey, m ())
newHoney honeyServerOptions honeyOptions defaultFields = do
    (sendQueue, shutdown) <- newTransport honeyServerOptions
    pure (mkHoney honeyOptions defaultFields sendQueue, shutdown)

{- |
Creates a Honey environment, and if given a program that uses this,
will run the program with an environment, correctly shutting everything
down afterwards.
-}
withHoney
    :: MonadUnliftIO m
    => HoneyServerOptions  -- ^ Options for how event handling is performed
    -> HoneyOptions        -- ^ Options for client library behaviour
    -> HoneyObject         -- ^ Default fields added to all events
    -> (Honey -> m a)      -- ^ The program to run
    -> m a
withHoney honeyServerOptions honeyOptions honeyFields inner = withRunInIO $ \run ->
    bracket (newHoney honeyServerOptions honeyOptions honeyFields)
            snd
            (run . inner . fst)
