{-# LANGUAGE ScopedTypeVariables #-}

module Honeycomb.Core
  ( honeyOptionsFromEnv,
    withHoney,
    withHoney',
    newHoney,
    newHoney',
    withHoneyOptions,

    -- * Forcing event sending
    flush,
  )
where

import Control.Monad.Reader (MonadReader, local, void)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Honeycomb.Core.Internal.Types
import Honeycomb.Core.Types
import Honeycomb.Transport
import Lens.Micro (over, (&), (.~))
import Lens.Micro.Mtl (view)
import System.Environment (lookupEnv)
import UnliftIO

-- | Waits until all currently sent events have been dequeued and processed.
--
-- This may be useful in a system which suspends processing when idle; the user
-- may want to guarantee that all queued events have been sent.
--
-- This only guarantees that events queued before this call will be sent. A
-- user may add more events afterwards, and this does not guarantee that those
-- events have been sent.
flush ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasHoney env
  ) =>
  -- | Length of time to wait before giving up (in microseconds)
  Int ->
  m ()
flush timeout_us = do
  flushQueue <- view $ honeyL . honeyTransportStateL . transportFlushQueueL
  mvar <- newEmptyTMVarIO
  atomically $ writeTBQueue flushQueue mvar
  void $ timeout timeout_us $ atomically $ takeTMVar mvar

-- | Creates a new Honey library instance.
--
-- A background thread is started up, which will dequeue events that
-- need to be sent. On shutdown, the event queue is shut down, and
-- the background thread stops once all messages are processed.
--
-- Discovers Honey options from the environment, using 'honeyOptionsFromEnv';
-- if you wish to set the options manually, use 'newHoney''
newHoney ::
  ( MonadUnliftIO n,
    MonadIO m
  ) =>
  n (Honey, m ())
newHoney = do
  honeyServerOptions <- defaultHoneyServerOptions
  honeyOptions <- honeyOptionsFromEnv
  (transportState, shutdown) <- newTransport honeyServerOptions
  pure (mkHoney honeyOptions transportState, shutdown)

-- | Creates a new Honey library instance.
--
-- A background thread is started up, which will dequeue events that
-- need to be sent. On shutdown, the event queue is shut down, and
-- the background thread stops once all messages are processed.
newHoney' ::
  ( MonadUnliftIO n,
    MonadIO m
  ) =>
  -- | Options for how event handling is performed
  HoneyServerOptions n ->
  -- | Options for client library behaviour
  HoneyOptions ->
  n (Honey, m ())
newHoney' honeyServerOptions honeyOptions = do
  (transportState, shutdown) <- newTransport honeyServerOptions
  pure (mkHoney honeyOptions transportState, shutdown)

-- |
-- Creates a Honey environment, and if given a program that uses this,
-- will run the program with an environment, correctly shutting everything
-- down afterwards.
--
-- Discovers Honey options from the environment; if you wish to set the
-- options manually, use 'withHoney'' or 'withHoneyOptions'
withHoney ::
  MonadUnliftIO m =>
  -- | The program to run
  (Honey -> m a) ->
  m a
withHoney inner = withRunInIO $ \run ->
  bracket
    newHoney
    snd
    (run . inner . fst)

-- |
-- Creates a Honey environment, and if given a program that uses this,
-- will run the program with an environment, correctly shutting everything
-- down afterwards.
withHoney' ::
  MonadUnliftIO m =>
  -- | Options for how event handling is performed
  HoneyServerOptions m ->
  -- | Options for client library behaviour
  HoneyOptions ->
  -- | The program to run
  (Honey -> m a) ->
  m a
withHoney' honeyServerOptions honeyOptions inner =
  bracket
    (newHoney' honeyServerOptions honeyOptions)
    snd
    (inner . fst)

-- | Modifies the HoneyOptions value for the provided program.
--
-- This allows a program to be run, with a @HoneyOptions@ value which is different
-- to the one configured when setting up the library.
withHoneyOptions ::
  ( MonadReader env m,
    HasHoney env
  ) =>
  -- | The function to modify the current options value
  (HoneyOptions -> HoneyOptions) ->
  -- | The program to run
  m a ->
  m a
withHoneyOptions f = local (over (honeyL . honeyOptionsL) f)

-- | Gets options for the library from the process environment.
--
-- This reads the default API Key from @HONEYCOMB_API_KEY@.
--
-- It reads the default dataset from @HONEYCOMB_DATASET@.
--
-- In addition, if @HONEYCOMB_DISABLED@ is set to any value, no
-- Honeycomb events are queued or sent (but no errors are raised).
honeyOptionsFromEnv ::
  MonadIO m =>
  m HoneyOptions
honeyOptionsFromEnv = do
  apiKeyEnv <- liftIO $ fmap (ApiKey . T.pack) <$> lookupEnv "HONEYCOMB_API_KEY"
  datasetEnv <- liftIO $ fmap (Dataset . T.pack) <$> lookupEnv "HONEYCOMB_DATASET"
  disabledEnv <- liftIO $ lookupEnv "HONEYCOMB_DISABLED"
  pure $
    defaultHoneyOptions
      & apiKeyL .~ apiKeyEnv
      & datasetL .~ datasetEnv
      & disabledL .~ isJust disabledEnv
