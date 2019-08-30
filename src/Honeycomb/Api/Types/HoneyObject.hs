{-# LANGUAGE FlexibleInstances #-}
module Honeycomb.Api.Types.HoneyObject where

import Honeycomb.Api.Types.HoneyValue

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type HoneyObject = HM.HashMap T.Text HoneyValue

class ToHoneyObject a where
    toHoneyObject :: a -> HoneyObject

instance ToHoneyValue v => ToHoneyObject (HM.HashMap T.Text v) where
    toHoneyObject kvs = toHoneyValue <$> kvs

instance ToHoneyValue v => ToHoneyObject [(T.Text, v)] where
    toHoneyObject kvs = HM.fromList $ (\(k, v) -> (k, toHoneyValue v)) <$> kvs
