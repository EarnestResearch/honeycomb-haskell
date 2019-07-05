module Network.Monitoring.Honeycomb.Types.HoneyServerOptions
    ( HoneyServerOptions
    , blockOnResponseL
    , maxBatchSizeL
    , sendFrequencyL
    , maxConcurrentBatchesL
    , pendingWorkCapacityL
    , defaultHoneyServerOptions
    , honeyServerOptionsL
    ) where

import Numeric.Natural (Natural)
import RIO
import RIO.Time

data HoneyServerOptions = HoneyServerOptions
    { blockOnResponse      :: !Bool
    , maxBatchSize         :: !Natural
    , sendFrequency        :: !DiffTime
    , maxConcurrentBatches :: !Natural
    , pendingWorkCapacity  :: !Natural
    } deriving (Show)

blockOnResponseL :: Lens' HoneyServerOptions Bool
blockOnResponseL = lens blockOnResponse (\x y -> x { blockOnResponse = y })

maxBatchSizeL :: Lens' HoneyServerOptions Natural
maxBatchSizeL = lens maxBatchSize (\x y -> x { maxBatchSize = y })

sendFrequencyL :: Lens' HoneyServerOptions DiffTime
sendFrequencyL = lens sendFrequency (\x y -> x { sendFrequency = y })

maxConcurrentBatchesL :: Lens' HoneyServerOptions Natural
maxConcurrentBatchesL = lens maxConcurrentBatches (\x y -> x { maxConcurrentBatches = y })

pendingWorkCapacityL :: Lens' HoneyServerOptions Natural
pendingWorkCapacityL = lens pendingWorkCapacity (\x y -> x { pendingWorkCapacity = y })

class HasHoneyServerOptions env where
    honeyServerOptionsL :: Lens' env HoneyServerOptions

instance HasHoneyServerOptions HoneyServerOptions where
    honeyServerOptionsL = id

defaultMaxBatchSize :: Natural
defaultMaxBatchSize = 50

defaultBatchTimeout :: DiffTime
defaultBatchTimeout = picosecondsToDiffTime 100000000000  -- 100ms

defaultMaxConcurrentBatches :: Natural
defaultMaxConcurrentBatches = 80

defaultPendingWorkCapacity :: Natural
defaultPendingWorkCapacity = 10000

defaultHoneyServerOptions :: HoneyServerOptions
defaultHoneyServerOptions = HoneyServerOptions
    { blockOnResponse = False
    , maxBatchSize = defaultMaxBatchSize
    , sendFrequency = defaultBatchTimeout
    , maxConcurrentBatches = defaultMaxConcurrentBatches
    , pendingWorkCapacity = defaultPendingWorkCapacity
    }
