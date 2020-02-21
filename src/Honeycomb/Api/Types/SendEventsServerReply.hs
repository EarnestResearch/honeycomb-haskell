{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Api.Types.SendEventsServerReply where

import qualified Data.Aeson as JSON
import Data.Aeson ((.:), (.=))
import qualified Data.Text as T

-- | Raw response
--
-- This represents the raw response to each event sent by the
-- 'Honeycomb.Api.sendEvents' call. There should be one of these structures
-- for each event sent in the batch call.
data SendEventsServerReply
  = SendEventsServerReply
      { serverReplyStatus :: !Int,
        serverReplyError :: !(Maybe T.Text)
      }
  deriving (Show)

instance JSON.ToJSON SendEventsServerReply where
  toJSON reply =
    JSON.object
      [ "status" .= serverReplyStatus reply,
        "error" .= serverReplyError reply
      ]

instance JSON.FromJSON SendEventsServerReply where
  parseJSON = JSON.withObject "SendEventsServerReply" $ \v ->
    SendEventsServerReply
      <$> v .: "status"
      <*> v .: "error"
