module Network.Monitoring.Honeycomb.Trace.Types.SpanContext
    ( SpanContext (..)
    , HasSpanContext
    , spanContextL
    )
where

import Network.Monitoring.Honeycomb.Trace.Types.SpanId
import Network.Monitoring.Honeycomb.Trace.Types.SpanName
import Network.Monitoring.Honeycomb.Trace.Types.SpanReference
import Network.Monitoring.Honeycomb.Trace.Types.TraceId
import Network.Monitoring.Honeycomb.Types.HoneyEvent
import RIO

data SpanContext = SpanContext
    { spanReference :: !SpanReference
    , parentSpanId  :: !(Maybe SpanId)
    , spanName      :: !SpanName
    , spanEvent     :: !HoneyEvent
    } deriving (Show)

instance HasSpanReference SpanContext where
    getSpanReference = spanReference

instance HasHoneyEvent SpanContext where
    getHoneyEvent = Just . spanEvent

class HasSpanContext env where
    spanContextL :: Lens' env (Maybe SpanContext)

instance HasSpanContext (Maybe SpanContext) where
    spanContextL = id

instance HasTraceId SpanContext where
    getTraceId = getTraceId . spanReference

instance HasSpanId SpanContext where
    getSpanId = getSpanId . spanReference
