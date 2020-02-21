-- |
-- Module      : Honeycomb.Api
-- Description : API access library for @honeycomb.io@
-- Copyright   : (c) 2019-2020 Earnest Research
-- License     : Apache-2
-- Maintainer  : gcoady@earnestresearch.com
-- Stability   : alpha
-- Portability : POSIX
--
-- The "Honeycomb.Api" module provides a very low-level method of accessing
-- the Honeycomb API. Each call represents a direct request to the Honeycomb
-- servers (with limited support for retrying on timeout).
module Honeycomb.Api
  ( module Honeycomb.Api.Events,
    module Honeycomb.Api.Types,
  )
where

import Honeycomb.Api.Events
import Honeycomb.Api.Types
