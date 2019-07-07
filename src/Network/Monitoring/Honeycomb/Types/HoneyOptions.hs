module Network.Monitoring.Honeycomb.Types.HoneyOptions
    ( HoneyOptions
    , apiKeyL
    , datasetL
    , sampleRateL
    , apiHostL
    , defaultFieldsL
    , defaultHoneyOptions
    , mergeOptionsWithBuilder
    ) where

import Network.Monitoring.Honeycomb.Types.ApiKey
import Network.Monitoring.Honeycomb.Types.Dataset
import Network.Monitoring.Honeycomb.Types.HoneyEventBuilder
import Network.Monitoring.Honeycomb.Types.HoneyObject
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
    } deriving (Show)

apiKeyL :: Lens' HoneyOptions (Maybe ApiKey)
apiKeyL = lens apiKey (\x y -> x { apiKey = y})

datasetL :: Lens' HoneyOptions (Maybe Dataset)
datasetL = lens dataset (\x y -> x { dataset = y})

sampleRateL :: Lens' HoneyOptions Natural
sampleRateL = lens sampleRate (\x y -> x { sampleRate = y})

apiHostL :: Lens' HoneyOptions URI.URI
apiHostL = lens apiHost (\x y -> x { apiHost = y})

defaultFieldsL :: Lens' HoneyOptions HoneyObject
defaultFieldsL = lens defaultFields (\x y -> x { defaultFields = y })

defaultHoneyOptions :: HoneyOptions
defaultHoneyOptions = HoneyOptions
    { apiKey = Nothing
    , dataset = Nothing
    , sampleRate = 1
    , apiHost = fromJust $ URI.parseURI "https://api.honeycomb.io/"
    , defaultFields = HM.empty
    }

mergeOptionsWithBuilder :: HoneyEventBuilder -> HoneyOptions -> HoneyOptions
mergeOptionsWithBuilder builder opts = HoneyOptions
    { apiKey = builder ^. builderApiKeyL <|> opts ^. apiKeyL
    , dataset = builder ^. builderDatasetL <|> opts ^. datasetL
    , sampleRate = fromMaybe (opts ^. sampleRateL) (builder ^. builderSampleRateL)
    , apiHost = fromMaybe (opts ^. apiHostL) (builder ^. builderApiHostL)
    , defaultFields = fromMaybe (opts ^. defaultFieldsL) (builder ^. builderDefaultFieldsL)
    }