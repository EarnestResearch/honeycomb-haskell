module Honeycomb.Types.HoneyException
    ( HoneyException (..)
    ) where

import RIO

data HoneyException
    = MissingApiKeyOption
    | MissingDatasetOption
    | EmptyEventData
    deriving (Eq, Show)

instance Exception HoneyException

