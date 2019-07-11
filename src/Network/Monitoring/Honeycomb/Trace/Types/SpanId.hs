module Network.Monitoring.Honeycomb.Trace.Types.SpanId
    ( SpanId (..)
    , HasSpanId
    , ToSpanId
    , mkSpanId
    , getSpanId
    , toSpanId
    )
where

import Data.Coerce (coerce)
import Network.Monitoring.Honeycomb.Types
import RIO

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4

newtype SpanId = SpanId Text deriving (Eq, Show)

mkSpanId :: MonadIO m => m SpanId
mkSpanId = toSpanId <$> liftIO V4.nextRandom

class ToSpanId a where
  toSpanId :: a -> SpanId

instance ToSpanId Text where
  toSpanId = SpanId

instance ToSpanId UUID.UUID where
  toSpanId = SpanId . UUID.toText

class HasSpanId a where
  getSpanId :: a -> SpanId

instance HasSpanId SpanId where
  getSpanId = id

instance ToHoneyValue SpanId where
  toHoneyValue = HoneyString . coerce