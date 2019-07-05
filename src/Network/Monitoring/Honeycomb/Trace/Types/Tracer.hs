module Network.Monitoring.Honeycomb.Trace.Types.Tracer
    ( Tracer
    , HasTracer
    , serviceNameL
    , tracerL
    , propagationL
    , mkTracer
    )
 where

import Network.Monitoring.Honeycomb.Trace.Types.SpanContext
import Network.Monitoring.Honeycomb.Trace.Types.Propagation
import Network.Monitoring.Honeycomb.Types.HoneyEvent
import RIO

data Tracer = Tracer
  { serviceName  :: !Text
  , propagation  :: !Propagation
  , spanContext  :: !(Maybe SpanContext)
  }

mkTracer :: Text -> Tracer
mkTracer serviceName = Tracer
    { serviceName
    , propagation = mkNoopPropagation
    , spanContext = Nothing
    }

serviceNameL :: Lens' Tracer Text
serviceNameL = lens serviceName (\x y -> x { serviceName = y })

propagationL :: Lens' Tracer Propagation
propagationL = lens propagation (\x y -> x { propagation = y })

class HasSpanContext env => HasTracer env where
    tracerL :: Lens' env Tracer

instance HasTracer Tracer where
    tracerL = id

instance HasHoneyEvent Tracer where
    getHoneyEvent t = spanContext t >>= getHoneyEvent
    
instance HasSpanContext Tracer where
    spanContextL = lens spanContext (\x y -> x { spanContext = y })
