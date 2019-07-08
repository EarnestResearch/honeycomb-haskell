module Network.Monitoring.Honeycomb.Api.Types.ApiKey
    ( ApiKey (..)
    ) where

import RIO

newtype ApiKey = ApiKey Text deriving (Eq, IsString, Ord, Show)
