{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Api.Types.SendEventsServerReply where

import qualified Data.Aeson as JSON
import Data.Aeson ((.:))
import qualified Data.Text as T

data SendEventsServerReply
  = SendEventsServerReply
      { serverReplyStatus :: !Int,
        serverReplyError :: !(Maybe T.Text)
      }
  deriving (Show)

instance JSON.FromJSON SendEventsServerReply where
  parseJSON = JSON.withObject "SendEventsServerReply" $ \v ->
    SendEventsServerReply
      <$> v .: "status"
      <*> v .: "error"
