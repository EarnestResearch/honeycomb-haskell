{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Api.Types.HoneyField where

import qualified Data.Text as T
import Honeycomb.Api.Types.HoneyValue
import GHC.Generics

newtype HoneyField = HoneyField (T.Text, HoneyValue)
  deriving (Eq, Show)

class ToHoneyField k where
  toHoneyField :: T.Text -> k -> HoneyField

instance ToHoneyValue v => ToHoneyField (T.Text, v) where
  toHoneyField prefix (k, v) = HoneyField ((prefix <> ".") <> k, toHoneyValue v)

instance ToHoneyValue v => ToHoneyField ([Char], v) where
  toHoneyField prefix (k, v) = HoneyField ((prefix <> ".") <> T.pack k, toHoneyValue v)

class GToHoneyField f where
  gToHoneyField :: T.Text -> f a -> HoneyField

instance (GToHoneyValue a, Selector s) => GToHoneyField (M1 S s a) where
  gToHoneyField prefix s@(M1 x) = HoneyField ((prefix <> ".") <> T.pack (selName s), gToHoneyValue x)
