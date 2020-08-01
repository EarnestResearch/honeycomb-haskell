{-# LANGUAGE StrictData #-}
module Honeycomb.Trace.TraceHeaderFormat.Types (TraceHeaderFormat(..)) where

import Honeycomb.Trace.Types.SpanReference
import Network.HTTP.Types (HeaderName)
import qualified Data.Text as T

data TraceHeaderFormat = TraceHeaderFormat {
  parseTraceHeader :: [(HeaderName, T.Text)] -> Maybe SpanReference
, writeTraceHeader :: SpanReference -> [(HeaderName, T.Text)]
}