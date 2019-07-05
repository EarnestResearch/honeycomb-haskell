module Network.Monitoring.Honeycomb.Types.ApiKey
    ( ApiKey (..)
    ) where

import RIO

newtype ApiKey = ApiKey Text deriving (IsString, Show)
