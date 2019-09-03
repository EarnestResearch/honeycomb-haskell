module Honeycomb.Core.Types.TransportState
    ( TransportState
    , transportSendQueueL
    , transportResponseQueueL
    , transportFlushQueueL
    , mkTransportState
    ) where

import Honeycomb.Core.Types.HoneyResponse
import Lens.Micro (Lens', lens)
import Numeric.Natural (Natural)
import UnliftIO

import qualified Honeycomb.Api.Types as Api

data TransportState = TransportState
    { transportSendQueue     :: !(TBQueue (Api.RequestOptions, Api.Event))
    , transportResponseQueue :: !(TBQueue HoneyResponse)
    , transportFlushQueue    :: !(TBQueue (TMVar ()))  -- do not expect a reply if library has shut down
    } deriving (Eq)

transportSendQueueL :: Lens' TransportState (TBQueue (Api.RequestOptions, Api.Event))
transportSendQueueL = lens transportSendQueue (\x y -> x { transportSendQueue = y })

transportResponseQueueL :: Lens' TransportState (TBQueue HoneyResponse)
transportResponseQueueL = lens transportResponseQueue (\x y -> x { transportResponseQueue = y })

transportFlushQueueL :: Lens' TransportState (TBQueue (TMVar ()))
transportFlushQueueL = lens transportFlushQueue (\x y -> x { transportFlushQueue = y })
    
mkTransportState :: MonadIO m => Natural -> Natural -> m TransportState
mkTransportState maxSendQueueSize maxResponseQueueSize =
    TransportState
        <$> newTBQueueIO maxSendQueueSize
        <*> newTBQueueIO maxResponseQueueSize
        <*> newTBQueueIO 100

instance Show TransportState where
    show _ = "TransportState {...}"
