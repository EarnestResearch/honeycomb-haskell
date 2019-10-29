module Honeycomb.Core.Types.Honey
    ( Honey (..)
    , mkHoney
    , honeyOptionsL
    , honeyTransportStateL
    , HasHoney
    , honeyL
    ) where

import Lens.Micro (Lens', lens)
import Honeycomb.Core.Types.HoneyOptions
import Honeycomb.Core.Types.TransportState

data Honey = Honey
    { honeyOptions        :: !HoneyOptions
    , honeyTransportState :: !TransportState
    } deriving (Eq, Show)

honeyOptionsL :: Lens' Honey HoneyOptions
honeyOptionsL = lens honeyOptions (\x y -> x { honeyOptions = y })

honeyTransportStateL :: Lens' Honey TransportState
honeyTransportStateL = lens honeyTransportState (\x y -> x { honeyTransportState = y })

mkHoney :: HoneyOptions -> TransportState -> Honey
mkHoney = Honey

class HasHoney env where
    honeyL :: Lens' env Honey

instance HasHoney Honey where
    honeyL = id
