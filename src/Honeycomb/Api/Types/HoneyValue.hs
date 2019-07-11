module Honeycomb.Api.Types.HoneyValue
    ( HoneyValue (..)
    , ToHoneyValue
    , toHoneyValue
    ) where

import Data.Scientific (Scientific, fromFloatDigits)
import RIO
import RIO.Time

import qualified Data.Aeson as JSON
import qualified RIO.Text as Text

data HoneyValue
    = HoneyNull
    | HoneyString !Text
    | HoneyNumber !Scientific
    | HoneyBool !Bool
    deriving (Eq, Show)

class ToHoneyValue a where
    toHoneyValue :: a -> HoneyValue

instance ToHoneyValue HoneyValue where
    toHoneyValue = id

instance ToHoneyValue Int where
    toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue Integer where
    toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue Natural where
    toHoneyValue = HoneyNumber . fromIntegral

instance ToHoneyValue NominalDiffTime where
    toHoneyValue = HoneyNumber . (* 1000) . realToFrac

instance ToHoneyValue UTCTime where
    toHoneyValue = HoneyString . Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

instance ToHoneyValue Double where
    toHoneyValue = HoneyNumber . fromFloatDigits

instance ToHoneyValue Float where
    toHoneyValue = HoneyNumber . fromFloatDigits

instance ToHoneyValue Scientific where
    toHoneyValue = HoneyNumber

instance ToHoneyValue Text where
    toHoneyValue = HoneyString

instance ToHoneyValue [Char] where
    toHoneyValue = HoneyString . Text.pack

instance IsString HoneyValue where
    fromString = HoneyString . Text.pack

instance JSON.ToJSON HoneyValue where
    toJSON (HoneyNull)      = JSON.Null
    toJSON (HoneyString s)  = JSON.String s
    toJSON (HoneyNumber n)  = JSON.Number n
    toJSON (HoneyBool b)    = JSON.Bool b