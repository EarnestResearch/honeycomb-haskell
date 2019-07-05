module Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
    ( HoneyQueueMessage (..)
    ) where

import Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
import RIO

data HoneyQueueMessage
    = QueueMessage !FrozenHoneyEvent
    | CloseQueue
    | FlushQueue !(MVar ())
