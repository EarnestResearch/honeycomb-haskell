{-# LANGUAGE FlexibleInstances #-}

module Honeycomb.Api.Types.HoneyValue
  ( HoneyValue (..),
    ToHoneyValue,
    toHoneyValue,
  )
where

import qualified Data.Aeson as JSON
import Data.Scientific (Scientific, fromFloatDigits)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric.Natural (Natural)

data HoneyValue
  = HoneyNull
  | HoneyString !T.Text
  | HoneyNumber !Scientific
  | HoneyBool !Bool
  deriving (Eq, Show)

class ToHoneyValue a where
  toHoneyValue :: a -> HoneyValue

instance ToHoneyValue HoneyValue where
  toHoneyValue = id

instance ToHoneyValue Bool where
  toHoneyValue = HoneyBool

instance ToHoneyValue Int where
  toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue Integer where
  toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue Natural where
  toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue NominalDiffTime where
  toHoneyValue = HoneyNumber . (* 1000) . realToFrac

instance ToHoneyValue UTCTime where
  toHoneyValue = HoneyString . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

instance ToHoneyValue Double where
  toHoneyValue = HoneyNumber . fromFloatDigits

instance ToHoneyValue Float where
  toHoneyValue = HoneyNumber . fromFloatDigits

instance ToHoneyValue Scientific where
  toHoneyValue = HoneyNumber

instance ToHoneyValue T.Text where
  toHoneyValue = HoneyString

instance ToHoneyValue [Char] where
  toHoneyValue = HoneyString . T.pack

instance IsString HoneyValue where
  fromString = HoneyString . T.pack

instance JSON.ToJSON HoneyValue where
  toJSON HoneyNull = JSON.Null
  toJSON (HoneyString s) = JSON.String s
  toJSON (HoneyNumber n) = JSON.Number n
  toJSON (HoneyBool b) = JSON.Bool b
