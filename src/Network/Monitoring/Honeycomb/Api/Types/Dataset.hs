module Network.Monitoring.Honeycomb.Api.Types.Dataset
    ( Dataset (..)
    ) where

import RIO

newtype Dataset = Dataset Text deriving (Eq, IsString, Ord, Show)
