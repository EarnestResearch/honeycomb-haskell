module Network.Monitoring.Honeycomb.Types.HoneyResponse
    ( HoneyResponse (..)
    , honeyResponseStatusL
    , honeyResponseLatencyL
    , honeyResponseErrorL
    ) where

import RIO
import RIO.Time

data HoneyResponse = HoneyResponse
    { honeyResponseStatus  :: !Int
    , honeyResponseLatency :: !NominalDiffTime
    , honeyResponseError   :: !(Maybe Text)
    } deriving (Eq, Show)

honeyResponseStatusL :: Lens' HoneyResponse Int
honeyResponseStatusL = lens honeyResponseStatus (\x y -> x { honeyResponseStatus = y })

honeyResponseLatencyL :: Lens' HoneyResponse NominalDiffTime
honeyResponseLatencyL = lens honeyResponseLatency (\x y -> x { honeyResponseLatency = y })

honeyResponseErrorL :: Lens' HoneyResponse (Maybe Text)
honeyResponseErrorL = lens honeyResponseError (\x y -> x { honeyResponseError = y })