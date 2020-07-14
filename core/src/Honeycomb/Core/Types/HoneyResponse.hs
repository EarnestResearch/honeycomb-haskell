{-# LANGUAGE RankNTypes #-}

module Honeycomb.Core.Types.HoneyResponse
  ( HoneyResponse (..),
    honeyResponseMetadataL,
    honeyResponseStatusCodeL,
    honeyResponseErrorL,
  )
where

import Data.Dynamic (Dynamic)
import qualified Data.Text as T
import Lens.Micro (Lens', lens)

data HoneyResponse
  = HoneyResponse
      { honeyResponseMetadata :: !(Maybe Dynamic),
        honeyResponseStatusCode :: !(Maybe Int),
        honeyResponseError :: !(Maybe T.Text)
      }
  deriving (Show)

honeyResponseMetadataL :: Lens' HoneyResponse (Maybe Dynamic)
honeyResponseMetadataL = lens honeyResponseMetadata (\x y -> x {honeyResponseMetadata = y})

honeyResponseStatusCodeL :: Lens' HoneyResponse (Maybe Int)
honeyResponseStatusCodeL = lens honeyResponseStatusCode (\x y -> x {honeyResponseStatusCode = y})

honeyResponseErrorL :: Lens' HoneyResponse (Maybe T.Text)
honeyResponseErrorL = lens honeyResponseError (\x y -> x {honeyResponseError = y})
