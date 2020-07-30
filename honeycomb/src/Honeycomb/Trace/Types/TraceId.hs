{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Honeycomb.Trace.Types.TraceId
  ( TraceId (..),
    HasTraceId,
    ToTraceId,
    mkTraceId,
    toTraceId,
    getTraceId,
  )
where

import Data.Coerce (coerce)
import qualified Data.Text as T
import Honeycomb.Core.Types
import System.Random (randomIO)
import UnliftIO
import Numeric (showHex)
import Data.Word (Word64)

newtype TraceId = TraceId T.Text deriving (Eq, Show)

mkTraceId :: MonadIO m => m TraceId
mkTraceId = do
  (b1 :: Word64) <- liftIO randomIO
  (b2 :: Word64) <- liftIO randomIO
  pure . TraceId . T.pack . showHex b1 $ showHex b2  ""

class ToTraceId a where
  toTraceId :: a -> TraceId

instance ToTraceId T.Text where
  toTraceId = TraceId

instance ToTraceId (Word64, Word64) where
  toTraceId (w1, w2) = TraceId . T.pack . showHex w1 $ showHex w2 ""

class HasTraceId a where
  getTraceId :: a -> TraceId

instance HasTraceId TraceId where
  getTraceId = id

instance ToHoneyValue TraceId where
  toHoneyValue = HoneyString . coerce
