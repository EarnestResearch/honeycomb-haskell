{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Api.Types.Event
  ( Event,

    -- * Create an event
    mkEvent,

    -- * Lenses
    eventFieldsL,
    eventMetadataL,
    eventSampleRateL,
    eventTimestampL,
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON
import Data.Dynamic (Dynamic)
import Data.Time (UTCTime)
import Honeycomb.Api.Types.HoneyObject
import Lens.Micro (Lens', (^.), lens)
import Numeric.Natural (Natural)

-- | Honeycomb Event
--
-- Represents the data which is sent to Honeycomb services for
-- each event (unless dropped or sampled).
data Event
  = Event
      { eventMetadata :: !(Maybe Dynamic),
        eventFields :: !HoneyObject,
        eventTimestamp :: !(Maybe UTCTime),
        eventSampleRate :: !(Maybe Natural)
      }
  deriving (Show)

-- | Lens to access the metadata of the event.
eventMetadataL :: Lens' Event (Maybe Dynamic)
eventMetadataL = lens eventMetadata (\x y -> x {eventMetadata = y})

-- | Lens to access the fields of the event.
eventFieldsL :: Lens' Event HoneyObject
eventFieldsL = lens eventFields (\x y -> x {eventFields = y})

-- | Lens to access the timestamp of the event.
eventTimestampL :: Lens' Event (Maybe UTCTime)
eventTimestampL = lens eventTimestamp (\x y -> x {eventTimestamp = y})

-- | Lens to access the sample rate of the event.
eventSampleRateL :: Lens' Event (Maybe Natural)
eventSampleRateL = lens eventSampleRate (\x y -> x {eventSampleRate = y})

-- | Create an API Event value from a "HoneyObject"
--
-- This creates a simple event (for the API) from an object created by the user.
mkEvent :: HoneyObject -> Event
mkEvent obj = Event {eventMetadata = Nothing, eventFields = obj, eventTimestamp = Nothing, eventSampleRate = Nothing}

instance JSON.ToJSON Event where
  toJSON event =
    JSON.object
      [ "data" .= (event ^. eventFieldsL),
        "time" .= (event ^. eventTimestampL),
        "samplerate" .= (event ^. eventSampleRateL)
      ]

instance JSON.FromJSON Event where
  parseJSON = JSON.withObject "Event" $ \v ->
    Event Nothing
      <$> v .: "data"
      <*> v .: "time"
      <*> v .: "samplerate"
