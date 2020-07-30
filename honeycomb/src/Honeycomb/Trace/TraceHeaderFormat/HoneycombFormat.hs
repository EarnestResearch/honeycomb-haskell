{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Trace.TraceHeaderFormat.HoneycombFormat
  ( honeycombTraceHeaderFormat,
  )
where

import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Honeycomb.Trace.Types as HC
import Honeycomb.Trace.TraceHeaderFormat.Types
import Network.HTTP.Types (HeaderName)

readHoneycombTraceHeader :: [(HeaderName, T.Text)] -> Maybe HC.SpanReference
readHoneycombTraceHeader headers = do
  (_, headerValue) <- List.find (\(name, _) -> name == "X-Honeycomb-Trace") headers
  headerText <- getV1 headerValue
  let parts = T.split (== '=') <$> T.split (== ',') headerText
  (_ : tid) <- List.find (getVal "trace_id") parts
  (_ : sid) <- List.find (getVal "parent_id") parts
  pure $ HC.SpanReference (HC.TraceId $ T.intercalate "=" tid) (HC.SpanId $ T.intercalate "=" sid)

getVal :: T.Text -> [T.Text] -> Bool
getVal header = \case
  [] -> False
  [_] -> False
  h : _ -> h == header

getV1 :: T.Text -> Maybe T.Text
getV1 t =
  if T.take 2 t == "1;"
    then Just $ T.drop 2 t
    else Nothing

writeHoneycombTraceHeader :: HC.SpanReference -> [(HeaderName, T.Text)]
writeHoneycombTraceHeader sr =
  [("X-Honeycomb-Trace", spanReferenceToText)]
  where
    spanReferenceToText :: T.Text
    spanReferenceToText = "1;trace_id=" <> coerce (HC.getTraceId sr) <> ",parent_id=" <> coerce (HC.getSpanId sr)

honeycombTraceHeaderFormat :: TraceHeaderFormat
honeycombTraceHeaderFormat =
  TraceHeaderFormat
    { parseTraceHeader = readHoneycombTraceHeader,
      writeTraceHeader = writeHoneycombTraceHeader
    }
