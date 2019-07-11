module Honeycomb.Api.Types.ApiKey
    ( ApiKey (..)
    ) where

import RIO

{- | Honeycomb API key

API keys allow you to send events to Honeycomb as well as manage markers, triggers,
and boards. A single API key can apply to multiple datasets.
-}
newtype ApiKey = ApiKey Text deriving (Eq, IsString, Ord, Show)
