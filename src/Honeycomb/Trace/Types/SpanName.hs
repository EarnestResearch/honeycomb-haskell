{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Honeycomb.Trace.Types.SpanName where

import Data.Coerce (coerce)
import Data.String (IsString)
import Honeycomb.Core.Types

import qualified Data.Text as T

newtype SpanName = SpanName T.Text deriving (Eq, IsString, Show)

instance ToHoneyValue SpanName where
    toHoneyValue = HoneyString . coerce
