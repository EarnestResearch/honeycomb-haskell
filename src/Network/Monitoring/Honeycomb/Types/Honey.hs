module Network.Monitoring.Honeycomb.Types.Honey
    ( Honey
    , mkHoney
    , honeyOptionsL
    , honeyServerOptionsL
    , sendQueueL
    , HasHoney
    , honeyL
    ) where

import Network.Monitoring.Honeycomb.Types.HoneyOptions
import Network.Monitoring.Honeycomb.Types.HoneyServerOptions
import Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
import RIO

data Honey = Honey
    { honeyServerOptions :: !HoneyServerOptions
    , honeyOptions       :: !HoneyOptions
    , sendQueue          :: !(TBQueue HoneyQueueMessage)
    }

honeyServerOptionsL :: Lens' Honey HoneyServerOptions
honeyServerOptionsL = lens honeyServerOptions (\x y -> x { honeyServerOptions = y })
    
honeyOptionsL :: Lens' Honey HoneyOptions
honeyOptionsL = lens honeyOptions (\x y -> x { honeyOptions = y })

sendQueueL :: Lens' Honey (TBQueue HoneyQueueMessage)
sendQueueL = lens sendQueue (\x y -> x { sendQueue = y })

mkHoney :: HoneyServerOptions -> HoneyOptions -> TBQueue HoneyQueueMessage -> Honey
mkHoney = Honey

class HasHoney env where
    honeyL :: Lens' env Honey

instance HasHoney Honey where
    honeyL = id
