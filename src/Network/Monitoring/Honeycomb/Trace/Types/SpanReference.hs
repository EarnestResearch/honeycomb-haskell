module Network.Monitoring.Honeycomb.Trace.Types.SpanReference
    ( SpanReference
    , HasSpanReference
    , mkSpanReference
    , getSpanReference
    )
where

import Network.Monitoring.Honeycomb.Trace.Types.SpanId
import Network.Monitoring.Honeycomb.Trace.Types.TraceId
import RIO

data SpanReference = SpanReference
    { refTraceId :: !TraceId
    , refSpanId  :: !SpanId
    } deriving (Show)

mkSpanReference :: TraceId -> SpanId -> SpanReference
mkSpanReference = SpanReference

class HasSpanReference a where
    getSpanReference :: a -> SpanReference

instance HasSpanReference SpanReference where
    getSpanReference = id

instance HasTraceId SpanReference where
    getTraceId = refTraceId

instance HasSpanId SpanReference where
    getSpanId = refSpanId
