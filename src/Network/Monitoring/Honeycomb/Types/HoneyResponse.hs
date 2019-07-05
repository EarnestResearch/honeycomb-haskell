module Network.Monitoring.Honeycomb.Types.HoneyResponse
    ( HoneyResponse (..)
    ) where

import RIO
import RIO.Time

data HoneyResponse = HoneyResponse
    { status  :: !Int
    , latency :: !NominalDiffTime
    , error   :: !(Maybe Text)
    } deriving (Show)
