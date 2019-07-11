module Network.Monitoring.Honeycomb.Api.Types.Event
    ( Event
    , mkEvent
    , eventFieldsL
    , eventTimestampL
    , eventSampleRateL
    ) where

import Data.Aeson ((.=))
import Network.Monitoring.Honeycomb.Api.Types.HoneyObject

import RIO
import RIO.Time

import qualified Data.Aeson as JSON

data Event = Event
    { eventFields     :: !HoneyObject
    , eventTimestamp  :: !(Maybe UTCTime)
    , eventSampleRate :: !(Maybe Natural)
    } deriving (Eq, Show)

eventFieldsL :: Lens' Event HoneyObject
eventFieldsL = lens eventFields (\x y -> x { eventFields = y })
    
eventTimestampL :: Lens' Event (Maybe UTCTime)
eventTimestampL = lens eventTimestamp (\x y -> x { eventTimestamp = y })

eventSampleRateL :: Lens' Event (Maybe Natural)
eventSampleRateL = lens eventSampleRate (\x y -> x { eventSampleRate = y })

mkEvent :: HoneyObject -> Maybe UTCTime -> Maybe Natural -> Event
mkEvent = Event

instance JSON.ToJSON Event where
    toJSON event = JSON.object
        [ "data" .= (event ^. eventFieldsL)
        , "time" .= (event ^. eventTimestampL)
        , "samplerate" .= (event ^. eventSampleRateL)
        ]
