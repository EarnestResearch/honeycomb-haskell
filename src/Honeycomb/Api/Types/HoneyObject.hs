{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Api.Types.HoneyObject
  ( HoneyObject (..),
    ToHoneyObject,
    ToHoneyValue,
    toHoneyObject,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Honeycomb.Api.Types.HoneyValue

newtype HoneyObject
  = HoneyObject (HM.HashMap T.Text HoneyValue)
  deriving (Eq, Show, JSON.ToJSON, JSON.FromJSON)

instance Monoid HoneyObject where
  mempty = HoneyObject HM.empty

instance Semigroup HoneyObject where
  (<>) m1 m2 = HoneyObject $ HM.union (coerce m1) (coerce m2)

class ToHoneyObject a where
  toHoneyObject :: a -> HoneyObject

instance ToHoneyObject HoneyObject where
  toHoneyObject = id

instance ToHoneyValue v => ToHoneyObject (HM.HashMap T.Text v) where
  toHoneyObject kvs = HoneyObject $ toHoneyValue <$> kvs

instance ToHoneyValue v => ToHoneyObject [(T.Text, v)] where
  toHoneyObject kvs = HoneyObject $ HM.fromList $ second toHoneyValue <$> kvs
