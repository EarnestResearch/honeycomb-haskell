module Honeycomb.Core.Types.HoneyServerOptions
  ( HoneyServerOptions,
    blockOnResponseL,
    maxBatchSizeL,
    sendFrequencyL,
    maxConcurrentBatchesL,
    pendingWorkCapacityL,
    httpLbsL,
    defaultHoneyServerOptions,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Lens.Micro (Lens', lens)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Numeric.Natural (Natural)
import UnliftIO

data HoneyServerOptions m
  = HoneyServerOptions
      { blockOnResponse :: !Bool,
        maxBatchSize :: !Natural,
        -- | Frequency of batch sending in milliseconds
        sendFrequency :: !Int,
        maxConcurrentBatches :: !Natural,
        pendingWorkCapacity :: !Natural,
        httpLbs :: Client.Request -> m (Client.Response LBS.ByteString)
      }

instance Show (HoneyServerOptions m) where
  show _ = "HoneyServerOptions {...}"

blockOnResponseL :: Lens' (HoneyServerOptions m) Bool
blockOnResponseL = lens blockOnResponse (\x y -> x {blockOnResponse = y})

maxBatchSizeL :: Lens' (HoneyServerOptions m) Natural
maxBatchSizeL = lens maxBatchSize (\x y -> x {maxBatchSize = y})

sendFrequencyL :: Lens' (HoneyServerOptions m) Int
sendFrequencyL = lens sendFrequency (\x y -> x {sendFrequency = y})

maxConcurrentBatchesL :: Lens' (HoneyServerOptions m) Natural
maxConcurrentBatchesL = lens maxConcurrentBatches (\x y -> x {maxConcurrentBatches = y})

pendingWorkCapacityL :: Lens' (HoneyServerOptions m) Natural
pendingWorkCapacityL = lens pendingWorkCapacity (\x y -> x {pendingWorkCapacity = y})

httpLbsL :: Lens' (HoneyServerOptions m) (Client.Request -> m (Client.Response LBS.ByteString))
httpLbsL = lens httpLbs (\x y -> x {httpLbs = y})

defaultMaxBatchSize :: Natural
defaultMaxBatchSize = 50

defaultBatchTimeout :: Int -- [todo] why is this not sendFrequency, Kenneth?
defaultBatchTimeout = 100 -- 100 ms

defaultMaxConcurrentBatches :: Natural
defaultMaxConcurrentBatches = 80

defaultPendingWorkCapacity :: Natural
defaultPendingWorkCapacity = 10000

defaultHoneyServerOptions :: MonadUnliftIO m => m (HoneyServerOptions m)
defaultHoneyServerOptions = do
  manager <- liftIO $ Client.newManager tlsManagerSettings
  pure $ HoneyServerOptions
    { blockOnResponse = False,
      maxBatchSize = defaultMaxBatchSize,
      sendFrequency = defaultBatchTimeout,
      maxConcurrentBatches = defaultMaxConcurrentBatches,
      pendingWorkCapacity = defaultPendingWorkCapacity,
      httpLbs = liftIO . flip Client.httpLbs manager
    }
