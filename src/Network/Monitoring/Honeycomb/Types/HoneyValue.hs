module Network.Monitoring.Honeycomb.Types.HoneyValue
    ( HoneyValue (..)
    , ToHoneyValue
    , toHoneyValue
    ) where

import Data.Scientific (Scientific, fromFloatDigits)
import qualified GHC.Exts as GHCExts
import Numeric.Natural (Natural)
import RIO
import RIO.Time

import qualified Data.Aeson as Aeson
import qualified RIO.Text as Text

data HoneyValue
    = HoneyStringValue Text
    | HoneyNumericValue Scientific
    | HoneyBoolValue Bool
    deriving (Show)

class ToHoneyValue a where
    toHoneyValue :: a -> HoneyValue

instance ToHoneyValue HoneyValue where
    toHoneyValue = id

instance ToHoneyValue Int where
    toHoneyValue = HoneyNumericValue . fromIntegral

instance ToHoneyValue Integer where
    toHoneyValue = HoneyNumericValue . fromIntegral

instance ToHoneyValue Natural where
    toHoneyValue = HoneyNumericValue . fromIntegral

instance ToHoneyValue NominalDiffTime where
    toHoneyValue = HoneyNumericValue . (* 1000) . realToFrac

instance ToHoneyValue UTCTime where
    toHoneyValue = HoneyStringValue . Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

instance ToHoneyValue Double where
    toHoneyValue = HoneyNumericValue . fromFloatDigits

instance ToHoneyValue Float where
    toHoneyValue = HoneyNumericValue . fromFloatDigits

instance ToHoneyValue Scientific where
    toHoneyValue = HoneyNumericValue

instance ToHoneyValue Text where
    toHoneyValue = HoneyStringValue

instance ToHoneyValue [Char] where
    toHoneyValue = HoneyStringValue . Text.pack

instance IsString HoneyValue where
    fromString = HoneyStringValue . Text.pack

instance Aeson.ToJSON HoneyValue where
    toJSON (HoneyStringValue s)  = Aeson.String s
    toJSON (HoneyNumericValue n) = Aeson.Number n
    toJSON (HoneyBoolValue b)    = Aeson.Bool b