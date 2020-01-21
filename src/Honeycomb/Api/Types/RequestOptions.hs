{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Honeycomb.Api.Types.RequestOptions
  ( RequestOptions,
    mkRequestOptions,
    requestApiHostL,
    requestApiDatasetL,
    requestApiKeyL,
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Honeycomb.Api.Types.ApiHost
import Honeycomb.Api.Types.ApiKey
import Honeycomb.Api.Types.Dataset
import Lens.Micro (Lens', lens)

data RequestOptions
  = RequestOptions
      { requestApiHost :: !ApiHost,
        requestApiDataset :: !Dataset,
        requestApiKey :: !ApiKey
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

mkRequestOptions :: ApiHost -> Dataset -> ApiKey -> RequestOptions
mkRequestOptions = RequestOptions

requestApiHostL :: Lens' RequestOptions ApiHost
requestApiHostL = lens requestApiHost (\x y -> x {requestApiHost = y})

requestApiDatasetL :: Lens' RequestOptions Dataset
requestApiDatasetL = lens requestApiDataset (\x y -> x {requestApiDataset = y})

requestApiKeyL :: Lens' RequestOptions ApiKey
requestApiKeyL = lens requestApiKey (\x y -> x {requestApiKey = y})
