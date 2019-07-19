module Honeycomb.Types.HoneyServerOptions
    ( HoneyServerOptions
    , blockOnResponseL
    , maxBatchSizeL
    , sendFrequencyL
    , maxConcurrentBatchesL
    , pendingWorkCapacityL
    , defaultHoneyServerOptions
    ) where

import Numeric.Natural (Natural)
import RIO

data HoneyServerOptions = HoneyServerOptions
    { blockOnResponse      :: !Bool
    , maxBatchSize         :: !Natural
    , sendFrequency        :: !Int      -- ^ Frequency of batch sending in milliseconds
    , maxConcurrentBatches :: !Natural
    , pendingWorkCapacity  :: !Natural
    } deriving (Eq, Show)

blockOnResponseL :: Lens' HoneyServerOptions Bool
blockOnResponseL = lens blockOnResponse (\x y -> x { blockOnResponse = y })

maxBatchSizeL :: Lens' HoneyServerOptions Natural
maxBatchSizeL = lens maxBatchSize (\x y -> x { maxBatchSize = y })

sendFrequencyL :: Lens' HoneyServerOptions Int
sendFrequencyL = lens sendFrequency (\x y -> x { sendFrequency = y })

maxConcurrentBatchesL :: Lens' HoneyServerOptions Natural
maxConcurrentBatchesL = lens maxConcurrentBatches (\x y -> x { maxConcurrentBatches = y })

pendingWorkCapacityL :: Lens' HoneyServerOptions Natural
pendingWorkCapacityL = lens pendingWorkCapacity (\x y -> x { pendingWorkCapacity = y })

defaultMaxBatchSize :: Natural
defaultMaxBatchSize = 50

defaultBatchTimeout :: Int -- [todo] why is this not sendFrequency, Kenneth?
defaultBatchTimeout = 100  -- 100 ms

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
