module Network.Monitoring.Honeycomb.Trace.Types.Tracer
    ( Tracer
    , HasTracer
    , serviceNameL
    , tracerL
    , propagationL
    , spanContextL
    , mkTracer
    )
 where

import Network.Monitoring.Honeycomb.Trace.Types.SpanContext
import Network.Monitoring.Honeycomb.Trace.Types.Propagation
import RIO

data Tracer = Tracer
  { serviceName  :: !Text
  , propagation  :: !Propagation
  , spanContext  :: !(Maybe SpanContext)
  }

serviceNameL :: Lens' Tracer Text
serviceNameL = lens serviceName (\x y -> x { serviceName = y })

propagationL :: Lens' Tracer Propagation
propagationL = lens propagation (\x y -> x { propagation = y })

spanContextL :: Lens' Tracer (Maybe SpanContext)
spanContextL = lens spanContext (\x y -> x { spanContext = y })

mkTracer :: Text -> Tracer
mkTracer serviceName = Tracer
    { serviceName
    , propagation = mkNoopPropagation
    , spanContext = Nothing
    }

class HasTracer env where
    tracerL :: Lens' env Tracer

instance HasTracer Tracer where
    tracerL = id
    
