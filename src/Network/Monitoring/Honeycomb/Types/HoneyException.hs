module Network.Monitoring.Honeycomb.Types.HoneyException
    ( HoneyException (..)
    ) where

import RIO

data HoneyException
    = MissingApiKeyOption
    | MissingDatasetOption
    | EmptyEventData
    deriving (Show)

instance Exception HoneyException

