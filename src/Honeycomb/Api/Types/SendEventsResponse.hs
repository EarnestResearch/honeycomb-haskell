module Honeycomb.Api.Types.SendEventsResponse where

import Honeycomb.Api.Types.Event
import Honeycomb.Api.Types.SendEventsServerReply
import qualified Network.HTTP.Client as Client

data SendEventsResponse
  = SendEventsResponse
      { unsentEvents :: ![Event],
        oversizedEvents :: ![Event],
        serviceResponse :: !(Client.Response (Maybe [SendEventsServerReply]))
      }
