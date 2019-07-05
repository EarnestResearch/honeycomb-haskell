module Network.Monitoring.Honeycomb.Trace.Types.SpanName where

import Data.Coerce (coerce)
import Network.Monitoring.Honeycomb.Types.HoneyValue
import RIO

newtype SpanName = SpanName Text deriving (IsString, Show)

instance ToHoneyValue SpanName where
    toHoneyValue = HoneyStringValue . coerce