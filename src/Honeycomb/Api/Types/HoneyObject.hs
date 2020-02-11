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

-- | Honeycomb Object
--
-- A set of keys and corresponding values that are considered
-- to be valid pieces of data to be sent to Honeycomb for each
-- event.
newtype HoneyObject
  = HoneyObject (HM.HashMap T.Text HoneyValue)
  deriving (Eq, Show, JSON.ToJSON, JSON.FromJSON)

instance Monoid HoneyObject where
  mempty = HoneyObject HM.empty

instance Semigroup HoneyObject where
  (<>) m1 m2 = HoneyObject $ HM.union (coerce m1) (coerce m2)

-- | Instances of this class represent the ability to turn values of the corresponding type into Honeycomb objects.
class ToHoneyObject a where
  -- | Converts the value into a "HoneyObject" Honeycomb object.
  toHoneyObject :: a -> HoneyObject

instance ToHoneyObject HoneyObject where
  toHoneyObject = id

instance ToHoneyValue v => ToHoneyObject (HM.HashMap T.Text v) where
  toHoneyObject kvs = HoneyObject $ toHoneyValue <$> kvs

instance ToHoneyValue v => ToHoneyObject [(T.Text, v)] where
  toHoneyObject kvs = HoneyObject $ HM.fromList $ second toHoneyValue <$> kvs
