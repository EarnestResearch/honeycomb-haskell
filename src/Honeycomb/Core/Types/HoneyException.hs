module Honeycomb.Core.Types.HoneyException
    ( HoneyException (..)
    ) where

import UnliftIO.Exception (Exception)

data HoneyException
    = MissingApiKeyOption
    | MissingDatasetOption
    | EmptyEventData
    deriving (Eq, Show)

instance Exception HoneyException

