module Network.Monitoring.Honeycomb.Trace.Types.Propagation where

import Network.HTTP.Types.Header
import Network.Monitoring.Honeycomb.Trace.Types.PropagationData
import RIO

data Propagation = Propagation
    { fromPropagationData :: PropagationData -> RequestHeaders
    , toPropagationData :: RequestHeaders -> Maybe PropagationData
    }

fromPropagationDataL :: Lens' Propagation (PropagationData -> RequestHeaders)
fromPropagationDataL = lens fromPropagationData (\x y -> x { fromPropagationData = y })

toPropagationDataL :: Lens' Propagation (RequestHeaders -> Maybe PropagationData)
toPropagationDataL = lens toPropagationData (\x y -> x { toPropagationData = y })

mkNoopPropagation :: Propagation
mkNoopPropagation = Propagation
    { fromPropagationData = const mempty
    , toPropagationData = const Nothing
    }