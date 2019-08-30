module Honeycomb.Types.HoneyResponse
    ( HoneyResponse (..)
    , honeyResponseStatusCodeL
    , honeyResponseBodyL
    , honeyResponseDurationL
    , honeyResponseErrorL
    ) where

import Data.Time.Clock (NominalDiffTime)
import Lens.Micro (Lens', lens)

import qualified Data.ByteString as BS
import qualified Data.Text as T

data HoneyResponse = HoneyResponse
    { honeyResponseMetadata :: ()
    , honeyResponseStatusCode  :: !Int
    , honeyResponseBody :: !BS.ByteString
    , honeyResponseDuration :: !NominalDiffTime
    , honeyResponseError   :: !(Maybe T.Text)
    } deriving (Eq, Show)

honeyResponseStatusCodeL :: Lens' HoneyResponse Int
honeyResponseStatusCodeL = lens honeyResponseStatusCode (\x y -> x { honeyResponseStatusCode = y })

honeyResponseBodyL :: Lens' HoneyResponse BS.ByteString
honeyResponseBodyL = lens honeyResponseBody (\x y -> x { honeyResponseBody = y })

honeyResponseDurationL :: Lens' HoneyResponse NominalDiffTime
honeyResponseDurationL = lens honeyResponseDuration (\x y -> x { honeyResponseDuration = y })

honeyResponseErrorL :: Lens' HoneyResponse (Maybe T.Text)
honeyResponseErrorL = lens honeyResponseError (\x y -> x { honeyResponseError = y })
