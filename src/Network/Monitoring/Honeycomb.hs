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
    , addField
    , addFieldSTM
    , addFields
    , addFieldsSTM

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
-- >     withHoney ho $ \appHoney ->
-- >         let app = App
-- >               { -- incude other app context/settings
-- >               , appHoney
-- >               }
-- >         in runRIO app run

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
 
-- | Adds a field to the event (in the STM monad).
--
--   This allows fields to be added to an event between its creation and
--   sending. Because it uses mutable variables, fields can be added
--   from different threads, and will be visible as long as the mutation
--   occurs before the event is sent.
addFieldSTM
    :: ( ToHoneyValue v
       )
    => Text
    -> v
    -> HoneyEvent
    -> STM ()
addFieldSTM k v evt =
    modifyTVar' (evt ^. eventFieldsL) $ HM.insert k (toHoneyValue v)

-- | Adds a field to the event.
--
--   This allows fields to be added to an event between its creation and
--   sending. Because it uses mutable variables, fields can be added
--   from different threads, and will be visible as long as the mutation
--   occurs before the event is sent.
addField
    :: ( MonadIO m
       , ToHoneyValue v
       )
    => Text
    -> v
    -> HoneyEvent
    -> m ()
addField k v =
    atomically . addFieldSTM k v

addFieldsSTM
    :: ( ToHoneyObject o
       )
    => o
    -> HoneyEvent
    -> STM ()
addFieldsSTM fields evt =
    modifyTVar' (evt ^. eventFieldsL) (toHoneyObject fields `HM.union`)

addFields
    :: ( MonadIO m
       , ToHoneyObject o
       )
    => o
    -> HoneyEvent
    -> m ()
addFields fields =
    atomically . addFieldsSTM fields

{- | Sends the event to Honeycomb service.

This takes a snapshot of the current fields, and sends the information
to the Honeycomb service.
-}
send
    :: ( MonadIO m
       , MonadReader env m
       , HasHoney env
       )
    => HoneyEvent
    -> m ()
send = send' (HM.empty :: HoneyObject)

send'
    :: forall m env o .
       ( MonadIO m
       , MonadReader env m
       , ToHoneyObject o
       , HasHoney env
       )
    => o
    -> HoneyEvent
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

-- | Waits until all currently sent events have been dequeued and processed.
--
--
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

newHoney
    :: ( MonadUnliftIO n
       , MonadIO m
       )
    => HoneyServerOptions
    -> HoneyOptions
    -> HoneyObject
    -> n (Honey, m ())
newHoney honeyServerOptions honeyOptions defaultFields = do
    (sendQueue, shutdown) <- newTransport honeyServerOptions
    pure (mkHoney honeyOptions defaultFields sendQueue, shutdown)

-- | Creates a Honey environment, and if given a program that uses this,
--   will run the program with an environment, correctly shutting everything
--   down afterwards.
withHoney
    :: MonadUnliftIO m
    => HoneyServerOptions
    -> HoneyOptions
    -> HoneyObject
    -> (Honey -> m a)
    -> m a
withHoney honeyServerOptions honeyOptions honeyFields inner = withRunInIO $ \run ->
    bracket (newHoney honeyServerOptions honeyOptions honeyFields)
            snd
            (run . inner . fst)
