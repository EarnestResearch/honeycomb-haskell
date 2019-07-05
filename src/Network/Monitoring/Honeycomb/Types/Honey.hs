module Network.Monitoring.Honeycomb.Types.Honey
    ( Honey
    , mkHoney
    , honeyOptionsL
    , defaultFieldsL
    , sendQueueL
    , HasHoney
    , honeyL
    ) where

import Network.Monitoring.Honeycomb.Types.HoneyObject
import Network.Monitoring.Honeycomb.Types.HoneyOptions
import Network.Monitoring.Honeycomb.Types.HoneyQueueMessage
import RIO

data Honey = Honey
    { honeyOptions  :: !HoneyOptions
    , defaultFields :: !HoneyObject
    , sendQueue     :: !(TBQueue HoneyQueueMessage)
    }

mkHoney :: HoneyOptions -> HoneyObject -> TBQueue HoneyQueueMessage -> Honey
mkHoney = Honey

honeyOptionsL :: Lens' Honey HoneyOptions
honeyOptionsL = lens honeyOptions (\x y -> x { honeyOptions = y })

defaultFieldsL :: Lens' Honey HoneyObject
defaultFieldsL = lens defaultFields (\x y -> x { defaultFields = y })

sendQueueL :: Lens' Honey (TBQueue HoneyQueueMessage)
sendQueueL = lens sendQueue (\x y -> x { sendQueue = y })

class HasHoney env where
    honeyL :: Lens' env Honey

instance HasHoney Honey where
    honeyL = id
