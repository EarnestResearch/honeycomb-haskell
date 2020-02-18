module Honeycomb.Api.Types.SendEventsResponse where

import Honeycomb.Api.Types.Event
import Honeycomb.Api.Types.SendEventsServerReply
import qualified Network.HTTP.Client as Client

-- | Response to 'Honeycomb.Api.sendEvents' call.
--
-- When the 'Honeycomb.Api.sendEvents' call is invoked, some events may be dropped
-- because they are too large, and some may not have been sent because the
-- cumulative payload is too large.
--
-- This response returns both of those types of object, as well as the
-- response details from Honeycomb servers.
data SendEventsResponse
  = SendEventsResponse
      { unsentEvents :: ![Event],
        oversizedEvents :: ![Event],
        serviceResponse :: !(Client.Response (Maybe [SendEventsServerReply]))
      }
