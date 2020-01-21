module Honeycomb.Trace.Types.TraceId
  ( TraceId (..),
    HasTraceId,
    ToTraceId,
    mkTraceId,
    toTraceId,
    getTraceId,
  )
where

import Control.Monad.Reader (MonadIO, liftIO)
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import Honeycomb.Core.Types

newtype TraceId = TraceId T.Text deriving (Eq, Show)

mkTraceId :: MonadIO m => m TraceId
mkTraceId = toTraceId <$> liftIO V4.nextRandom

class ToTraceId a where
  toTraceId :: a -> TraceId

instance ToTraceId T.Text where
  toTraceId = TraceId

instance ToTraceId UUID.UUID where
  toTraceId = TraceId . UUID.toText

class HasTraceId a where
  getTraceId :: a -> TraceId

instance HasTraceId TraceId where
  getTraceId = id

instance ToHoneyValue TraceId where
  toHoneyValue = HoneyString . coerce
