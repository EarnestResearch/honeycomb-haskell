{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Honeycomb.Api.Types.HoneyObject
  ( HoneyObject (..),
    ToHoneyObject,
    ToHoneyValue,
    toHoneyObject,
    Test(..)
  )
where

import qualified Data.Aeson as JSON
import Data.Aeson.Casing (snakeCase)
import Data.Bifunctor (bimap, Bifunctor (first))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Honeycomb.Api.Types.HoneyField
import Honeycomb.Api.Types.HoneyValue
import GHC.Generics
import Data.Coerce (coerce)

-- | Honeycomb Object
--
-- A set of keys and corresponding values that are considered
-- to be valid pieces of data to be sent to Honeycomb for each
-- event.
newtype HoneyObject
  = HoneyObject (V.Vector HoneyField)
  deriving (Eq, Semigroup, Monoid, Show)

instance JSON.ToJSON HoneyObject where
  toJSON (HoneyObject fields) = undefined -- JSON.toJSON $ bimap JSON.toJSON JSON.toJSON <$> (coerce fields)

instance JSON.FromJSON HoneyObject where
  parseJSON (JSON.Object fields) = undefined -- JSON.toJSON $ bimap JSON.toJSON JSON.toJSON <$> (coerce fields)


-- | Instances of this class represent the ability to turn values of the corresponding type into Honeycomb objects.
class ToHoneyObject a where
  -- | Converts the value into a "HoneyObject" Honeycomb object.
  toHoneyObject :: T.Text -> a -> HoneyObject
  default toHoneyObject :: (Generic a, GToHoneyObject (Rep a)) => T.Text -> a -> HoneyObject
  toHoneyObject prefix = gToHoneyObject prefix . from

instance ToHoneyObject HoneyObject where
  toHoneyObject _ = id

instance ToHoneyObject (V.Vector (T.Text, HoneyValue)) where
  toHoneyObject prefix kvs = HoneyObject (toHoneyField prefix <$> kvs)
  
instance ToHoneyObject (HM.HashMap T.Text HoneyValue) where
  toHoneyObject prefix kvs = toHoneyObject prefix $ V.fromList $ HM.toList kvs

class GToHoneyObject f where
  gToHoneyObject :: T.Text -> f a -> HoneyObject

instance GToHoneyObject U1 where
  gToHoneyObject _ U1 = mempty

instance ToHoneyObject a => GToHoneyObject (K1 i a) where
  gToHoneyObject prefix (K1 x) = toHoneyObject prefix x

instance (GToHoneyValue a, Selector s) => GToHoneyObject (M1 S s a) where
  gToHoneyObject prefix = HoneyObject . V.singleton . gToHoneyField prefix

instance (GToHoneyObject a) => GToHoneyObject (M1 D s a) where
  gToHoneyObject prefix (M1 x) = gToHoneyObject prefix x

instance (GToHoneyObject a) => GToHoneyObject (M1 C s a) where
  gToHoneyObject prefix (M1 x) = gToHoneyObject prefix x

-- Encode products
instance (GToHoneyObject a, GToHoneyObject b) => GToHoneyObject (a :*: b) where
  gToHoneyObject prefix (x :*: y) = gToHoneyObject prefix x <> gToHoneyObject prefix y

-- Encode sums
-- The name of the branch is encoded in the
instance (GToHoneyObject a, GToHoneyObject b) => GToHoneyObject (a :+: b) where
  gToHoneyObject prefix (L1 x) = gToHoneyObject prefix x
  gToHoneyObject prefix (R1 x) = gToHoneyObject prefix x

data Test2 = Test2 { c :: Int, d :: Int }
  deriving (Generic)

data Test = Test { a :: Int, b :: Test2 }
  deriving (Generic)

instance ToHoneyObject Test
instance ToHoneyObject Test2

-- >>> test Test 1 2
--
test :: Test -> HoneyObject
test = toHoneyObject T.empty
