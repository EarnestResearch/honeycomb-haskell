module Honeycomb.Core.Types.Honey
  ( Honey,
    mkHoney,
    honeyOptionsL,
    honeyTransportStateL,
    honeyResponseQueueL,
    HasHoney,
    honeyL,
  )
where

import Honeycomb.Core.Internal.Types.TransportState
import Honeycomb.Core.Types.HoneyOptions
import Honeycomb.Core.Types.HoneyResponse
import Lens.Micro (Lens', lens)
import UnliftIO

-- | The core structure containing the state and settings needed for Honeycomb
--
-- This contains all the state and settings for the basic Honeycomb library to
-- operate. Generally, this will be placed in a "MonadReader" environment, and then
-- accessed through the "HasHoney" typeclass.
data Honey
  = Honey
      { honeyOptions :: !HoneyOptions,
        honeyTransportState :: !TransportState
      }

instance Show Honey where
  show _ = "Honey {...}"

honeyOptionsL :: Lens' Honey HoneyOptions
honeyOptionsL = lens honeyOptions (\x y -> x {honeyOptions = y})

honeyTransportStateL :: Lens' Honey TransportState
honeyTransportStateL = lens honeyTransportState (\x y -> x {honeyTransportState = y})

honeyResponseQueueL :: Lens' Honey (TBQueue HoneyResponse)
honeyResponseQueueL = honeyTransportStateL . transportResponseQueueL

mkHoney :: HoneyOptions -> TransportState -> Honey
mkHoney = Honey

-- | This class defines an ability to access a "Honey" value inside a larger object.
--
-- When this is defined for a "MonadReader" environment, it allows the Honeycomb library
-- to operate within that environment
class HasHoney env where
  -- | The lens that provides access to the "Honey" value.
  honeyL :: Lens' env Honey

instance HasHoney Honey where
  honeyL = id
