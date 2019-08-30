{-# LANGUAGE NamedFieldPuns #-}
module Honeycomb.Types.HoneyEvent
    ( HoneyEvent
    , mkHoneyEvent
    , mkHoneyEvent'
    , eventTimestampL
    , eventFieldsL
    , eventOptionsL
    )
where

import Control.Monad.Reader (MonadIO, liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Honeycomb.Types.HoneyOptions
import Lens.Micro (Lens', lens, (^.))
import UnliftIO.STM (TVar, newTVarIO)

import qualified Honeycomb.Api as Api

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
    eventTimestamp <- liftIO getCurrentTime
    mkHoneyEvent' eventTimestamp honeyOptions

mkHoneyEvent' :: MonadIO m => UTCTime -> HoneyOptions -> m HoneyEvent
mkHoneyEvent' eventTimestamp eventOptions = do
    eventFields <- newTVarIO $ eventOptions ^. defaultFieldsL
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
