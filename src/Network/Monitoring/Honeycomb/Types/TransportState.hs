module Network.Monitoring.Honeycomb.Types.TransportState
    ( TransportState
    , transportSendQueueL
    , transportFlushQueueL
    , mkTransportState
    ) where

import RIO

import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent

data TransportState = TransportState
    { transportSendQueue :: !(TBQueue FrozenHoneyEvent)
    , transportFlushQueue :: !(TMVar (TMVar ()))  -- do not expect a reply if library has shut down
    }

transportSendQueueL :: Lens' TransportState (TBQueue FrozenHoneyEvent)
transportSendQueueL = lens transportSendQueue (\x y -> x { transportSendQueue = y })

transportFlushQueueL :: Lens' TransportState (TMVar (TMVar ()))
transportFlushQueueL = lens transportFlushQueue (\x y -> x { transportFlushQueue = y })

mkTransportState :: MonadIO m => Natural -> m TransportState
mkTransportState maxQueueSize =
  TransportState <$> newTBQueueIO maxQueueSize <*> newEmptyTMVarIO
