{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Api.Types.ApiKey
  ( ApiKey (..),
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString)
import qualified Data.Text as T

-- | Honeycomb API key
--
-- API keys allow you to send events to Honeycomb as well as manage markers, triggers,
-- and boards. A single API key can apply to multiple datasets.
newtype ApiKey = ApiKey T.Text deriving (Eq, Hashable, IsString, Ord, Show)
