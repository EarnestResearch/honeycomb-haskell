module Network.Monitoring.Honeycomb.Types.HoneyOptions
    ( HoneyOptions
    , apiKeyL
    , datasetL
    , sampleRateL
    , apiHostL
    , blockOnSendL
    , defaultHoneyOptions
    , HasHoneyOptions
    , honeyOptionsL
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

class HasHoneyOptions env where
    honeyOptionsL :: Lens' env HoneyOptions

instance HasHoneyOptions HoneyOptions where
    honeyOptionsL = id

defaultHoneyOptions :: HoneyOptions
defaultHoneyOptions = HoneyOptions
    { apiKey = Nothing
    , dataset = Nothing
    , sampleRate = 1
    , apiHost = fromJust $ URI.parseURI "https://api.honeycomb.io/"
    , blockOnSend = False
    }

apiKeyL' :: Lens' HoneyOptions (Maybe ApiKey)
apiKeyL' = lens apiKey (\x y -> x { apiKey = y})

apiKeyL :: HasHoneyOptions env => Lens' env (Maybe ApiKey)
apiKeyL = honeyOptionsL . apiKeyL'

datasetL' :: Lens' HoneyOptions (Maybe Dataset)
datasetL' = lens dataset (\x y -> x { dataset = y})

datasetL :: HasHoneyOptions env => Lens' env (Maybe Dataset)
datasetL = honeyOptionsL . datasetL'

sampleRateL' :: Lens' HoneyOptions Natural
sampleRateL' = lens sampleRate (\x y -> x { sampleRate = y})

sampleRateL :: HasHoneyOptions env => Lens' env Natural
sampleRateL = honeyOptionsL . sampleRateL'

apiHostL' :: Lens' HoneyOptions URI.URI
apiHostL' = lens apiHost (\x y -> x { apiHost = y})

apiHostL :: HasHoneyOptions env => Lens' env URI.URI
apiHostL = honeyOptionsL . apiHostL'

blockOnSendL' :: Lens' HoneyOptions Bool
blockOnSendL' = lens blockOnSend (\x y -> x { blockOnSend = y})

blockOnSendL :: HasHoneyOptions env => Lens' env Bool
blockOnSendL = honeyOptionsL . blockOnSendL'
