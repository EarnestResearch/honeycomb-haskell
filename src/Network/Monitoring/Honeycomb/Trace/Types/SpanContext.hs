module Network.Monitoring.Honeycomb.Trace.Types.SpanContext
    ( SpanContext (..)
    , spanReferenceL
    , parentSpanIdL
    , serviceNameL
    , spanNameL
    , spanEventL
    , inheritableFieldsL
    , HasSpanContext
    , spanContextL
    )
where

import Network.Monitoring.Honeycomb.Trace.Types.ServiceName
import Network.Monitoring.Honeycomb.Trace.Types.SpanId
import Network.Monitoring.Honeycomb.Trace.Types.SpanName
import Network.Monitoring.Honeycomb.Trace.Types.SpanReference
import Network.Monitoring.Honeycomb.Trace.Types.TraceId
import Network.Monitoring.Honeycomb.Types.HoneyEvent
import RIO

data SpanContext = SpanContext
    { spanReference     :: !SpanReference
    , parentSpanId      :: !(Maybe SpanId)
    , serviceName       :: !ServiceName
    , spanName          :: !SpanName
    , spanEvent         :: !HoneyEvent
    , inheritableFields :: !(Set Text)
    } deriving (Eq, Show)

spanReferenceL :: Lens' SpanContext SpanReference
spanReferenceL = lens spanReference (\x y -> x { spanReference = y })

parentSpanIdL :: Lens' SpanContext (Maybe SpanId)
parentSpanIdL = lens parentSpanId (\x y -> x { parentSpanId = y })

serviceNameL :: Lens' SpanContext ServiceName
serviceNameL = lens serviceName (\x y -> x { serviceName = y })

spanNameL :: Lens' SpanContext SpanName
spanNameL = lens spanName (\x y -> x { spanName = y })

spanEventL :: Lens' SpanContext HoneyEvent
spanEventL = lens spanEvent (\x y -> x { spanEvent = y })

inheritableFieldsL :: Lens' SpanContext (Set Text)
inheritableFieldsL = lens inheritableFields (\x y -> x { inheritableFields = y })

class HasSpanContext env where
    spanContextL :: Lens' env (Maybe SpanContext)

instance HasSpanContext (Maybe SpanContext) where
    spanContextL = id

instance HasTraceId SpanContext where
    getTraceId = getTraceId . spanReference

instance HasSpanId SpanContext where
    getSpanId = getSpanId . spanReference
