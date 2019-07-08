module Network.Monitoring.Honeycomb.Api.Types.HoneyValue
    ( HoneyValue (..)
    , ToHoneyValue
    , toHoneyValue
    ) where

import Numeric.Natural (Natural)
import RIO
import RIO.Time

import qualified Data.Aeson as JSON
import qualified RIO.Text as Text

data HoneyValue
    = HoneyNil
    | HoneyString !Text
    | HoneyBool !Bool
    | HoneyFloat !Float
    | HoneyDouble !Double
    | HoneyInt !Int
    deriving (Show)

class ToHoneyValue a where
    toHoneyValue :: a -> HoneyValue

instance ToHoneyValue HoneyValue where
    toHoneyValue = id

instance ToHoneyValue Int where
    toHoneyValue = HoneyInt

instance ToHoneyValue Integer where
    toHoneyValue = HoneyInt . fromIntegral

instance ToHoneyValue Natural where
    toHoneyValue = HoneyInt . fromIntegral

instance ToHoneyValue NominalDiffTime where
    toHoneyValue = HoneyDouble . (* 1000) . realToFrac

instance ToHoneyValue UTCTime where
    toHoneyValue = HoneyString . Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

instance ToHoneyValue Double where
    toHoneyValue = HoneyDouble

instance ToHoneyValue Float where
    toHoneyValue = HoneyFloat

instance ToHoneyValue Text where
    toHoneyValue = HoneyString

instance ToHoneyValue [Char] where
    toHoneyValue = HoneyString . Text.pack

instance IsString HoneyValue where
    fromString = HoneyString . Text.pack

instance JSON.ToJSON HoneyValue where
    toJSON HoneyNil = JSON.Null
    toJSON (HoneyString s) = JSON.toJSON s
    toJSON (HoneyBool b) = JSON.toJSON b
    toJSON (HoneyFloat f) = JSON.toJSON f
    toJSON (HoneyDouble d) = JSON.toJSON d
    toJSON (HoneyInt i) = JSON.toJSON i
