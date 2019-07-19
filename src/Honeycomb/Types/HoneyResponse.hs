module Honeycomb.Types.HoneyResponse
    ( HoneyResponse (..)
    , honeyResponseStatusCodeL
    , honeyResponseBodyL
    , honeyResponseDurationL
    , honeyResponseErrorL
    ) where

import RIO
import RIO.Time

data HoneyResponse = HoneyResponse
    { honeyResponseMetadata :: ()
    , honeyResponseStatusCode  :: !Int
    , honeyResponseBody :: !ByteString
    , honeyResponseDuration :: !NominalDiffTime
    , honeyResponseError   :: !(Maybe Text)
    } deriving (Eq, Show)

honeyResponseStatusCodeL :: Lens' HoneyResponse Int
honeyResponseStatusCodeL = lens honeyResponseStatusCode (\x y -> x { honeyResponseStatusCode = y })

honeyResponseBodyL :: Lens' HoneyResponse ByteString
honeyResponseBodyL = lens honeyResponseBody (\x y -> x { honeyResponseBody = y })

honeyResponseDurationL :: Lens' HoneyResponse NominalDiffTime
honeyResponseDurationL = lens honeyResponseDuration (\x y -> x { honeyResponseDuration = y })

honeyResponseErrorL :: Lens' HoneyResponse (Maybe Text)
honeyResponseErrorL = lens honeyResponseError (\x y -> x { honeyResponseError = y })