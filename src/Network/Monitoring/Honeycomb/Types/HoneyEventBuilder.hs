module Network.Monitoring.Honeycomb.Types.HoneyEventBuilder where


import Network.Monitoring.Honeycomb.Types.ApiKey
import Network.Monitoring.Honeycomb.Types.Dataset
import Network.Monitoring.Honeycomb.Types.HoneyObject
import RIO

import qualified Network.URI as URI
  
data HoneyEventBuilder = HoneyEventBuilder
    { builderApiKey        :: !(Maybe ApiKey)
    , builderDataset       :: !(Maybe Dataset)
    , builderSampleRate    :: !(Maybe Natural)
    , builderApiHost       :: !(Maybe URI.URI)
    , builderDefaultFields :: !(Maybe HoneyObject)
    } deriving (Show)

builderApiKeyL :: Lens' HoneyEventBuilder (Maybe ApiKey)
builderApiKeyL = lens builderApiKey (\x y -> x { builderApiKey = y})

builderDatasetL :: Lens' HoneyEventBuilder (Maybe Dataset)
builderDatasetL = lens builderDataset (\x y -> x { builderDataset = y})

builderSampleRateL :: Lens' HoneyEventBuilder (Maybe Natural)
builderSampleRateL = lens builderSampleRate (\x y -> x { builderSampleRate = y})

builderApiHostL :: Lens' HoneyEventBuilder (Maybe URI.URI)
builderApiHostL = lens builderApiHost (\x y -> x { builderApiHost = y})

builderDefaultFieldsL :: Lens' HoneyEventBuilder (Maybe HoneyObject)
builderDefaultFieldsL = lens builderDefaultFields (\x y -> x { builderDefaultFields = y })
