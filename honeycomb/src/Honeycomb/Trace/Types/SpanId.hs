{-# LANGUAGE ScopedTypeVariables #-}
module Honeycomb.Trace.Types.SpanId
  ( SpanId (..),
    HasSpanId,
    ToSpanId,
    mkSpanId,
    getSpanId,
    toSpanId,
  )
where

import Control.Monad.Reader (MonadIO, liftIO)
import Data.Coerce (coerce)
import qualified Data.Text as T
import Data.Word (Word64)
import Honeycomb.Core.Types
import Numeric (showHex)
import System.Random (randomIO)

newtype SpanId = SpanId T.Text deriving (Eq, Show)

mkSpanId :: MonadIO m => m SpanId
mkSpanId = do
  (bytes :: Word64) <- liftIO randomIO
  pure . SpanId . T.pack $ showHex bytes ""

class ToSpanId a where
  toSpanId :: a -> SpanId

instance ToSpanId T.Text where
  toSpanId = SpanId

instance ToSpanId Word64 where
  toSpanId word = SpanId . T.pack $ showHex word ""

class HasSpanId a where
  getSpanId :: a -> SpanId

instance HasSpanId SpanId where
  getSpanId = id

instance ToHoneyValue SpanId where
  toHoneyValue = HoneyString . coerce
