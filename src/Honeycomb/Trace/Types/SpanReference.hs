module Honeycomb.Trace.Types.SpanReference
    ( SpanReference (..)
    )
where

import Honeycomb.Trace.Types.SpanId
import Honeycomb.Trace.Types.TraceId
import RIO

data SpanReference = SpanReference !TraceId !SpanId deriving (Eq, Show)

instance HasTraceId SpanReference where
    getTraceId (SpanReference tid _) = tid

instance HasSpanId SpanReference where
    getSpanId (SpanReference _ sid) = sid
