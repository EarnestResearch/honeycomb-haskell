module Network.Monitoring.Honeycomb.Types.TransportState
    ( TransportState
    , transportSendQueueL
    , transportFlushQueueL
    , mkTransportState
    ) where

import RIO

import qualified Network.Monitoring.Honeycomb.Api.Types as Api

data TransportState = TransportState
    { transportSendQueue :: !(TBQueue (Api.RequestOptions, Api.Event))
    , transportFlushQueue :: !(TBQueue (TMVar ()))  -- do not expect a reply if library has shut down
    } deriving (Eq)

transportSendQueueL :: Lens' TransportState (TBQueue (Api.RequestOptions, Api.Event))
transportSendQueueL = lens transportSendQueue (\x y -> x { transportSendQueue = y })

transportFlushQueueL :: Lens' TransportState (TBQueue (TMVar ()))
transportFlushQueueL = lens transportFlushQueue (\x y -> x { transportFlushQueue = y })

mkTransportState :: MonadIO m => Natural -> m TransportState
mkTransportState maxQueueSize =
    TransportState <$> newTBQueueIO maxQueueSize <*> newTBQueueIO 100

instance Show TransportState where
    show _ = "TransportState {...}"
