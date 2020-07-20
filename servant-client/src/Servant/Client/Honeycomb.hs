{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Client.Honeycomb
  (
  )
where

import Control.Monad (join)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Kind (Type)
import qualified Data.Vault.Lazy as V
import qualified Honeycomb.Trace as HC
import Lens.Micro (over)
import qualified Network.Wai as Wai
import Servant.API ((:>))
import Servant.Client (HasClient)
import Servant.Honeycomb
import UnliftIO hiding (Handler)
import UnliftIO.Wai

data Honeycomb deriving (Typeable)
--instance RunClient m => HasClient m (Honeycomb :> api) where

--instance (HasClient ClientM api)
