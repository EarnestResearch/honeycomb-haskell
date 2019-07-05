module Network.Monitoring.Honeycomb.Trace.Types.SpanContext
    ( SpanContext (..)
    , spanReferenceL
    , parentSpanIdL
    , spanNameL
    , spanEventL
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

spanReferenceL :: Lens' SpanContext SpanReference
spanReferenceL = lens spanReference (\x y -> x { spanReference = y })

parentSpanIdL :: Lens' SpanContext (Maybe SpanId)
parentSpanIdL = lens parentSpanId (\x y -> x { parentSpanId = y })

spanNameL :: Lens' SpanContext SpanName
spanNameL = lens spanName (\x y -> x { spanName = y })

spanEventL :: Lens' SpanContext HoneyEvent
spanEventL = lens spanEvent (\x y -> x { spanEvent = y })

instance HasTraceId SpanContext where
    getTraceId = getTraceId . spanReference

instance HasSpanId SpanContext where
    getSpanId = getSpanId . spanReference
