{-# LANGUAGE NamedFieldPuns #-}

module Honeycomb.Core.Types.HoneyEvent
  ( HoneyEvent,
    mkHoneyEvent,
    mkHoneyEvent',
    eventTimestampL,
    eventMetadataL,
    eventFieldsL,
    eventOptionsL,
  )
where

import Control.Monad.Reader (MonadIO, liftIO)
import Data.Dynamic (Dynamic)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Honeycomb.Api as Api
import Honeycomb.Core.Types.HoneyOptions
import Lens.Micro (Lens', (^.), lens)
import UnliftIO.STM (TVar, newTVarIO)

data HoneyEvent
  = HoneyEvent
      { eventTimestamp :: !UTCTime,
        eventOptions :: !HoneyOptions,
        eventMetadata :: !(Maybe Dynamic),
        eventFields :: !(TVar Api.HoneyObject)
      }

eventTimestampL :: Lens' HoneyEvent UTCTime
eventTimestampL = lens eventTimestamp (\x y -> x {eventTimestamp = y})

eventOptionsL :: Lens' HoneyEvent HoneyOptions
eventOptionsL = lens eventOptions (\x y -> x {eventOptions = y})

eventMetadataL :: Lens' HoneyEvent (Maybe Dynamic)
eventMetadataL = lens eventMetadata (\x y -> x {eventMetadata = y})

eventFieldsL :: Lens' HoneyEvent (TVar Api.HoneyObject)
eventFieldsL = lens eventFields (\x y -> x {eventFields = y})

mkHoneyEvent :: MonadIO m => HoneyOptions -> m HoneyEvent
mkHoneyEvent honeyOptions = do
  eventTimestamp <- liftIO getCurrentTime
  mkHoneyEvent' eventTimestamp honeyOptions

mkHoneyEvent' :: MonadIO m => UTCTime -> HoneyOptions -> m HoneyEvent
mkHoneyEvent' eventTimestamp eventOptions = do
  eventFields <- newTVarIO $ eventOptions ^. defaultFieldsL
  pure
    HoneyEvent
      { eventTimestamp,
        eventOptions,
        eventMetadata = Nothing,
        eventFields
      }

instance Show HoneyEvent where
  show e =
    "HoneyEvent "
      ++ "{ eventTimestamp = "
      ++ show (eventTimestamp e)
      ++ ", eventOptions = "
      ++ show (eventOptions e)
      ++ ", eventMetadata = {Metadata}"
      ++ ", eventFields = <TVar HoneyFields>"
      ++ "}"
