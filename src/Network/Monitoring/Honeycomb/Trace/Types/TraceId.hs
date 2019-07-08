module Network.Monitoring.Honeycomb.Trace.Types.TraceId
    ( TraceId (..)
    , HasTraceId
    , ToTraceId
    , mkTraceId
    , toTraceId
    , getTraceId
    )
where

import Data.Coerce (coerce)
import Network.Monitoring.Honeycomb.Types
import RIO

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4

newtype TraceId = TraceId Text deriving (Show)

mkTraceId :: MonadIO m => m TraceId
mkTraceId = toTraceId <$> liftIO V4.nextRandom

class ToTraceId a where
  toTraceId :: a -> TraceId

instance ToTraceId Text where
  toTraceId = TraceId

instance ToTraceId UUID.UUID where
  toTraceId = TraceId . UUID.toText

class HasTraceId a where
  getTraceId :: a -> TraceId

instance HasTraceId TraceId where
  getTraceId = id

instance ToHoneyValue TraceId where
  toHoneyValue = HoneyString . coerce