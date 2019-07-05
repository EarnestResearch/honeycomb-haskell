module Network.Monitoring.Honeycomb.Types.HoneyOptions
    ( HoneyOptions
    , apiKeyL
    , datasetL
    , sampleRateL
    , apiHostL
    , blockOnSendL
    , defaultHoneyOptions
    ) where

import Network.Monitoring.Honeycomb.Types.ApiKey
import Network.Monitoring.Honeycomb.Types.Dataset
import RIO
import RIO.Partial (fromJust)

import qualified Network.URI as URI

data HoneyOptions = HoneyOptions
    { apiKey      :: !(Maybe ApiKey)
    , dataset     :: !(Maybe Dataset)
    , sampleRate  :: !Natural
    , apiHost     :: !URI.URI
    , blockOnSend :: Bool
    } deriving (Show)

apiKeyL :: Lens' HoneyOptions (Maybe ApiKey)
apiKeyL = lens apiKey (\x y -> x { apiKey = y})

datasetL :: Lens' HoneyOptions (Maybe Dataset)
datasetL = lens dataset (\x y -> x { dataset = y})

sampleRateL :: Lens' HoneyOptions Natural
sampleRateL = lens sampleRate (\x y -> x { sampleRate = y})

apiHostL :: Lens' HoneyOptions URI.URI
apiHostL = lens apiHost (\x y -> x { apiHost = y})

blockOnSendL :: Lens' HoneyOptions Bool
blockOnSendL = lens blockOnSend (\x y -> x { blockOnSend = y})

defaultHoneyOptions :: HoneyOptions
defaultHoneyOptions = HoneyOptions
    { apiKey = Nothing
    , dataset = Nothing
    , sampleRate = 1
    , apiHost = fromJust $ URI.parseURI "https://api.honeycomb.io/"
    , blockOnSend = False
    }