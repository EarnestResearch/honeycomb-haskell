{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Api.Types.Dataset
  ( Dataset (..),
  )
where

import Data.Hashable (Hashable)
import Data.String (IsString)
import qualified Data.Text as T

-- | Honeycomb Dataset
--
-- Datasets are partitioning units for your Honeycomb organisation.
-- Queries do not span datasets, and each dataset has its own space
-- quota.
newtype Dataset = Dataset T.Text deriving (Eq, Hashable, IsString, Ord, Show)
