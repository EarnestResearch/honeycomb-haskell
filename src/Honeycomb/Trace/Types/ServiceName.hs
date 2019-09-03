{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Honeycomb.Trace.Types.ServiceName where

import Data.Coerce (coerce)
import Data.String (IsString)
import Honeycomb.Core.Types

import qualified Data.Text as T
  
newtype ServiceName = ServiceName T.Text deriving (Eq, IsString, Show)
 
instance ToHoneyValue ServiceName where
    toHoneyValue = HoneyString . coerce
