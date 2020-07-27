{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Trace.Types.ServiceName where

import Data.Coerce (coerce)
import Data.String (IsString)
import qualified Data.Text as T
import Honeycomb.Core.Types

newtype ServiceName = ServiceName T.Text deriving (Eq, IsString, Show)

instance ToHoneyValue ServiceName where
  toHoneyValue = HoneyString . coerce
