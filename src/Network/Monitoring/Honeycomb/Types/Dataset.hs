module Network.Monitoring.Honeycomb.Types.Dataset
    ( Dataset (..)
    ) where

import RIO

newtype Dataset = Dataset Text deriving (IsString, Show)
