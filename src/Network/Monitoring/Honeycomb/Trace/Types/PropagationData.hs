module Network.Monitoring.Honeycomb.Trace.Types.PropagationData where

import Network.Monitoring.Honeycomb.Trace.Types.SpanId
import Network.Monitoring.Honeycomb.Trace.Types.SpanReference
import Network.Monitoring.Honeycomb.Trace.Types.TraceId
import RIO

newtype PropagationData = PropagationData
    { propagationSpanReference :: SpanReference
    } deriving (Show)

propagationSpanReferenceL :: Lens' PropagationData SpanReference
propagationSpanReferenceL = lens propagationSpanReference (\x y -> x { propagationSpanReference = y })

instance HasTraceId PropagationData where
    getTraceId = getTraceId . propagationSpanReference

instance HasSpanId PropagationData where
    getSpanId = getSpanId . propagationSpanReference    