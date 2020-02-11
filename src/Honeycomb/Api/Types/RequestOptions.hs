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

-- | Request Options
--
-- These parameters represent the set of options used as keys when
-- partitioning data to be forwarded to Honeycomb servers.
data RequestOptions
  = RequestOptions
      { requestApiHost :: !ApiHost,
        requestApiDataset :: !Dataset,
        requestApiKey :: !ApiKey
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | Provide all the details needed to create a "RequestOptions" value.
mkRequestOptions :: ApiHost -> Dataset -> ApiKey -> RequestOptions
mkRequestOptions = RequestOptions

-- | Lens to access the API host used by the library.
requestApiHostL :: Lens' RequestOptions ApiHost
requestApiHostL = lens requestApiHost (\x y -> x {requestApiHost = y})

-- | Lens to access the dataset used by the library.
requestApiDatasetL :: Lens' RequestOptions Dataset
requestApiDatasetL = lens requestApiDataset (\x y -> x {requestApiDataset = y})

-- | Lens to access the API key used by the library.
requestApiKeyL :: Lens' RequestOptions ApiKey
requestApiKeyL = lens requestApiKey (\x y -> x {requestApiKey = y})
