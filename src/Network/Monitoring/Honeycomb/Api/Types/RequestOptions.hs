module Network.Monitoring.Honeycomb.Api.Types.RequestOptions
    ( RequestOptions
    , mkRequestOptions
    , requestApiHostL
    , requestApiDatasetL
    , requestApiKeyL
    )
where

import Network.Monitoring.Honeycomb.Api.Types.ApiKey
import Network.Monitoring.Honeycomb.Api.Types.Dataset
import Network.URI (URI)
import RIO

data RequestOptions = RequestOptions
  { requestApiHost :: !URI
  , requestApiDataset :: !Dataset
  , requestApiKey :: !ApiKey
  } deriving (Eq, Ord, Show)

mkRequestOptions :: URI -> Dataset -> ApiKey -> RequestOptions
mkRequestOptions = RequestOptions

requestApiHostL :: Lens' RequestOptions URI
requestApiHostL = lens requestApiHost (\x y -> x { requestApiHost = y })

requestApiDatasetL :: Lens' RequestOptions Dataset
requestApiDatasetL = lens requestApiDataset (\x y -> x { requestApiDataset = y })

requestApiKeyL :: Lens' RequestOptions ApiKey
requestApiKeyL = lens requestApiKey (\x y -> x { requestApiKey = y })
