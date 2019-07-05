module Network.Monitoring.Honeycomb.Types.HoneyObject where

import Network.Monitoring.Honeycomb.Types.HoneyValue

import RIO

import qualified RIO.HashMap as HM

type HoneyField = (Text, HoneyValue)
type HoneyObject = HM.HashMap Text HoneyValue

class ToHoneyObject a where
    toHoneyObject :: a -> HoneyObject

instance ToHoneyValue v => ToHoneyObject (HashMap Text v) where
    toHoneyObject kvs = toHoneyValue <$> kvs

instance ToHoneyValue v => ToHoneyObject [(Text, v)] where
    toHoneyObject kvs = HM.fromList $ (\(k, v) -> (k, toHoneyValue v)) <$> kvs
