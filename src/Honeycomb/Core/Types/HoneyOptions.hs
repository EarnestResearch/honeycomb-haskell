{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Core.Types.HoneyOptions
    ( HoneyOptions
    , apiKeyL
    , datasetL
    , sampleRateL
    , apiHostL
    , defaultFieldsL
    , blockOnSendL
    , disabledL
    , defaultHoneyOptions
    ) where

import Honeycomb.Api.Types.ApiHost
import Honeycomb.Api.Types.ApiKey
import Honeycomb.Api.Types.Dataset
import Honeycomb.Api.Types.HoneyObject
import Lens.Micro (Lens', lens)
import Numeric.Natural (Natural)

import qualified Data.HashMap.Strict as HM

data HoneyOptions = HoneyOptions
    { apiKey        :: !(Maybe ApiKey)
    , dataset       :: !(Maybe Dataset)
    , sampleRate    :: !Natural
    , apiHost       :: !ApiHost
    , defaultFields :: !HoneyObject
    , blockOnSend   :: !Bool
    , disabled      :: !Bool
    } deriving (Eq, Show)

apiKeyL :: Lens' HoneyOptions (Maybe ApiKey)
apiKeyL = lens apiKey (\x y -> x { apiKey = y })

datasetL :: Lens' HoneyOptions (Maybe Dataset)
datasetL = lens dataset (\x y -> x { dataset = y })

sampleRateL :: Lens' HoneyOptions Natural
sampleRateL = lens sampleRate (\x y -> x { sampleRate = y })

apiHostL :: Lens' HoneyOptions ApiHost
apiHostL = lens apiHost (\x y -> x { apiHost = y })

defaultFieldsL :: Lens' HoneyOptions HoneyObject
defaultFieldsL = lens defaultFields (\x y -> x { defaultFields = y })

blockOnSendL :: Lens' HoneyOptions Bool
blockOnSendL = lens blockOnSend (\x y -> x { blockOnSend = y})

disabledL :: Lens' HoneyOptions Bool
disabledL = lens disabled (\x y -> x { disabled = y})

defaultHoneyOptions :: HoneyOptions
defaultHoneyOptions = HoneyOptions
    { apiKey = Nothing
    , dataset = Nothing
    , sampleRate = 1
    , apiHost = "https://api.honeycomb.io/"
    , defaultFields = HM.empty
    , blockOnSend = False
    , disabled = False
    }
