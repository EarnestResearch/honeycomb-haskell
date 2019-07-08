module Network.Monitoring.Honeycomb.Api.Types.Dataset
    ( Dataset (..)
    ) where

import RIO

{- | Honeycomb Dataset

Datasets are partitioning units for your Honeycomb organisation.
Queries do not span datasets, and each dataset has its own space
quota.
-}
newtype Dataset = Dataset Text deriving (Eq, IsString, Ord, Show)
