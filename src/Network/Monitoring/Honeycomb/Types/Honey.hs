module Network.Monitoring.Honeycomb.Types.Honey
    ( Honey
    , mkHoney
    , honeyOptionsL
    , sendQueueL
    , HasHoney
    , honeyL
    ) where

import Network.Monitoring.Honeycomb.Types.HoneyOptions
import Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
import RIO

data Honey = Honey
    { honeyOptions  :: !HoneyOptions
    , sendQueue     :: !(TBQueue HoneyQueueMessage)
    }

honeyOptionsL :: Lens' Honey HoneyOptions
honeyOptionsL = lens honeyOptions (\x y -> x { honeyOptions = y })

sendQueueL :: Lens' Honey (TBQueue HoneyQueueMessage)
sendQueueL = lens sendQueue (\x y -> x { sendQueue = y })

mkHoney :: HoneyOptions -> TBQueue HoneyQueueMessage -> Honey
mkHoney = Honey

class HasHoney env where
    honeyL :: Lens' env Honey

instance HasHoney Honey where
    honeyL = id
