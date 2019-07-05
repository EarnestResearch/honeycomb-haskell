module Network.Monitoring.Honeycomb.Types.FrozenHoneyEvent
    ( FrozenHoneyEvent
    , newFrozenEvent
    , frozenEventTimestampL
    , frozenEventFieldsL
    , frozenEventApiKeyL
    , frozenEventDatasetL
    , frozenEventSampleRateL
    , frozenEventApiHostL
    , frozenEventShouldSampleL
    ) where

import Network.Monitoring.Honeycomb.Types.ApiKey
import Network.Monitoring.Honeycomb.Types.Dataset
import Network.Monitoring.Honeycomb.Types.HoneyException
import Network.Monitoring.Honeycomb.Types.HoneyEvent
import Network.Monitoring.Honeycomb.Types.HoneyObject
import Network.Monitoring.Honeycomb.Types.HoneyOptions
import RIO
import RIO.Time
import System.Random (randomRIO)

import qualified Network.URI as URI
import qualified RIO.HashMap as HM

data FrozenHoneyEvent = FrozenHoneyEvent
    { frozenEventTimestamp    :: !UTCTime
    , frozenEventFields       :: !HoneyObject
    , frozenEventApiKey       :: !ApiKey
    , frozenEventDataset      :: !Dataset
    , frozenEventSampleRate   :: !Natural
    , frozenEventApiHost      :: !URI.URI
    , frozenEventShouldSample :: !Bool
    } deriving (Show)

frozenEventTimestampL :: Lens' FrozenHoneyEvent UTCTime
frozenEventTimestampL = lens frozenEventTimestamp (\x y -> x { frozenEventTimestamp = y })

frozenEventFieldsL :: Lens' FrozenHoneyEvent HoneyObject
frozenEventFieldsL = lens frozenEventFields (\x y -> x { frozenEventFields = y })

frozenEventApiKeyL :: Lens' FrozenHoneyEvent ApiKey
frozenEventApiKeyL = lens frozenEventApiKey (\x y -> x { frozenEventApiKey = y })

frozenEventDatasetL :: Lens' FrozenHoneyEvent Dataset
frozenEventDatasetL = lens frozenEventDataset (\x y -> x { frozenEventDataset = y })

frozenEventSampleRateL :: Lens' FrozenHoneyEvent Natural
frozenEventSampleRateL = lens frozenEventSampleRate (\x y -> x { frozenEventSampleRate = y })

frozenEventApiHostL :: Lens' FrozenHoneyEvent URI.URI
frozenEventApiHostL = lens frozenEventApiHost (\x y -> x { frozenEventApiHost = y })

frozenEventShouldSampleL :: Lens' FrozenHoneyEvent Bool
frozenEventShouldSampleL = lens frozenEventShouldSample (\x y -> x { frozenEventShouldSample = y })

newFrozenEvent :: MonadIO m => HoneyObject -> HoneyEvent -> m FrozenHoneyEvent
newFrozenEvent extraFields event = do
    let options = event ^. eventOptionsL
    frozenEventApiKey <- maybe (throwIO MissingApiKeyOption) pure $ options ^. apiKeyL
    frozenEventDataset <- maybe (throwIO MissingDatasetOption) pure $ options ^. datasetL
    let frozenEventSampleRate = options ^. sampleRateL
    frozenEventShouldSample <- fmap (== 1) $ liftIO $ randomRIO (1, toInteger frozenEventSampleRate)
    currentFields <- readTVarIO $ event ^. eventFieldsL
    let frozenEventFields = extraFields `HM.union` currentFields
    pure FrozenHoneyEvent
        { frozenEventTimestamp = event ^. eventTimestampL
        , frozenEventFields
        , frozenEventApiKey
        , frozenEventDataset
        , frozenEventApiHost = options ^. apiHostL
        , frozenEventSampleRate
        , frozenEventShouldSample
        }
