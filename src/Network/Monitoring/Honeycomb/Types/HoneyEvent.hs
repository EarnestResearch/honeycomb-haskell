module Network.Monitoring.Honeycomb.Types.HoneyEvent
    ( HoneyEvent
    , mkHoneyEvent
    , mkHoneyEvent'
    , eventTimestampL
    , eventFieldsL
    , eventOptionsL
    , HasHoneyEvent
    , getHoneyEvent
    ) where

import Network.Monitoring.Honeycomb.Types.HoneyObject
import Network.Monitoring.Honeycomb.Types.HoneyOptions
import RIO
import RIO.Time

data HoneyEvent = HoneyEvent
    { eventTimestamp :: !UTCTime
    , eventOptions   :: !HoneyOptions
    , eventFields    :: !(TVar HoneyObject) 
    }

mkHoneyEvent :: MonadIO m => HoneyOptions -> HoneyObject -> m HoneyEvent
mkHoneyEvent honeyOptions defaultFields = do
    eventTimestamp <- getCurrentTime
    mkHoneyEvent' eventTimestamp honeyOptions defaultFields

mkHoneyEvent' :: MonadIO m => UTCTime -> HoneyOptions -> HoneyObject -> m HoneyEvent
mkHoneyEvent' eventTimestamp eventOptions defaultFields = do
    eventFields <- newTVarIO defaultFields
    pure HoneyEvent
        { eventTimestamp
        , eventOptions
        , eventFields
        }

eventTimestampL :: Lens' HoneyEvent UTCTime
eventTimestampL = lens eventTimestamp (\x y -> x { eventTimestamp = y })

eventOptionsL :: Lens' HoneyEvent HoneyOptions
eventOptionsL = lens eventOptions (\x y -> x { eventOptions = y})

eventFieldsL :: Lens' HoneyEvent (TVar HoneyObject)
eventFieldsL = lens eventFields (\x y -> x { eventFields = y })

instance Show HoneyEvent where
    show e = "HoneyEvent " ++
        "{ eventTimestamp = " ++ show (eventTimestamp e) ++
        ", eventOptions = " ++ show (eventOptions e) ++
        ", eventFields = <TVar HoneyFields>" ++
        "}"

class HasHoneyEvent a where
    getHoneyEvent :: a -> Maybe HoneyEvent

instance HasHoneyEvent HoneyEvent where
    getHoneyEvent = Just
