module Network.Monitoring.Honeycomb.Types.HoneyOptions
    ( HoneyOptions
    , apiKeyL
    , datasetL
    , sampleRateL
    , apiHostL
    , defaultFieldsL
    , blockOnSendL
    , defaultHoneyOptions
    ) where

import Network.Monitoring.Honeycomb.Api.Types.ApiKey
import Network.Monitoring.Honeycomb.Api.Types.Dataset
import Network.Monitoring.Honeycomb.Api.Types.HoneyObject
import RIO
import RIO.Partial (fromJust)

import qualified Network.URI as URI
import qualified RIO.HashMap as HM

data HoneyOptions = HoneyOptions
    { apiKey        :: !(Maybe ApiKey)
    , dataset       :: !(Maybe Dataset)
    , sampleRate    :: !Natural
    , apiHost       :: !URI.URI
    , defaultFields :: !HoneyObject
    , blockOnSend   :: !Bool
    } deriving (Show)

apiKeyL :: Lens' HoneyOptions (Maybe ApiKey)
apiKeyL = lens apiKey (\x y -> x { apiKey = y })

datasetL :: Lens' HoneyOptions (Maybe Dataset)
datasetL = lens dataset (\x y -> x { dataset = y })

sampleRateL :: Lens' HoneyOptions Natural
sampleRateL = lens sampleRate (\x y -> x { sampleRate = y })

apiHostL :: Lens' HoneyOptions URI.URI
apiHostL = lens apiHost (\x y -> x { apiHost = y })

defaultFieldsL :: Lens' HoneyOptions HoneyObject
defaultFieldsL = lens defaultFields (\x y -> x { defaultFields = y })

blockOnSendL :: Lens' HoneyOptions Bool
blockOnSendL = lens blockOnSend (\x y -> x { blockOnSend = y})

defaultHoneyOptions :: HoneyOptions
defaultHoneyOptions = HoneyOptions
    { apiKey = Nothing
    , dataset = Nothing
    , sampleRate = 1
    , apiHost = fromJust $ URI.parseURI "https://api.honeycomb.io/"
    , defaultFields = HM.empty
    , blockOnSend = False
    }