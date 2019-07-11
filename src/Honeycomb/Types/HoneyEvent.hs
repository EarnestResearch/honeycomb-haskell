module Honeycomb.Types.HoneyEvent
    ( HoneyEvent
    , mkHoneyEvent
    , mkHoneyEvent'
    , eventTimestampL
    , eventFieldsL
    , eventOptionsL
    )
where

import Honeycomb.Types.HoneyOptions
import RIO
import RIO.Time

import qualified Honeycomb.Api as Api
import qualified RIO.HashMap as HM

data HoneyEvent = HoneyEvent
    { eventTimestamp :: !UTCTime
    , eventOptions   :: !HoneyOptions
    , eventFields    :: !(TVar Api.HoneyObject) 
    } deriving (Eq)

eventTimestampL :: Lens' HoneyEvent UTCTime
eventTimestampL = lens eventTimestamp (\x y -> x { eventTimestamp = y })

eventOptionsL :: Lens' HoneyEvent HoneyOptions
eventOptionsL = lens eventOptions (\x y -> x { eventOptions = y})

eventFieldsL :: Lens' HoneyEvent (TVar Api.HoneyObject)
eventFieldsL = lens eventFields (\x y -> x { eventFields = y })

mkHoneyEvent :: MonadIO m => HoneyOptions -> m HoneyEvent
mkHoneyEvent honeyOptions = do
    eventTimestamp <- getCurrentTime
    mkHoneyEvent' eventTimestamp honeyOptions

mkHoneyEvent' :: MonadIO m => UTCTime -> HoneyOptions -> m HoneyEvent
mkHoneyEvent' eventTimestamp eventOptions = do
    eventFields <- newTVarIO HM.empty
    pure HoneyEvent
        { eventTimestamp
        , eventOptions
        , eventFields
        }

instance Show HoneyEvent where
    show e = "HoneyEvent " ++
        "{ eventTimestamp = " ++ show (eventTimestamp e) ++
        ", eventOptions = " ++ show (eventOptions e) ++
        ", eventFields = <TVar HoneyFields>" ++
        "}"
