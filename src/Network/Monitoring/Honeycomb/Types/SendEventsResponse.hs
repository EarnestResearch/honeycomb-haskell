module Network.Monitoring.Honeycomb.Types.SendEventsResponse where

import RIO

import qualified Data.Aeson as JSON

type SendEventsResponse = [ SendEventsResponseItem ]

newtype SendEventsResponseItem = SendEventsResponseItem
    { status :: Integer
    } deriving (Generic, Show)

instance JSON.FromJSON SendEventsResponseItem