module Honeycomb.Trace.Types.ServiceName where

  import Data.Coerce (coerce)
  import Honeycomb.Types
  import RIO
  
  newtype ServiceName = ServiceName Text deriving (Eq, IsString, Show)
  
  instance ToHoneyValue ServiceName where
      toHoneyValue = HoneyString . coerce