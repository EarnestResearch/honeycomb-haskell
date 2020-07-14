{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Api.Types.ApiHost
  ( ApiHost (..),
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString)
import qualified Data.Text as T

-- | Honeycomb API host
newtype ApiHost = ApiHost T.Text deriving (Eq, Hashable, IsString, Ord, Show)
