{-# LANGUAGE OverloadedStrings #-}

module Honeycomb.Trace.TraceHeaderFormat.W3CFormat
  ( w3cTraceHeaderFormat,
  )
where

import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Text as T
import Honeycomb.Trace.TraceHeaderFormat.Types
import Network.HTTP.Types (HeaderName)
import Honeycomb.Trace.Types (SpanId (..), SpanReference (..), TraceId (..), getSpanId, getTraceId)

readW3CTraceHeader :: [(HeaderName, T.Text)] -> Maybe SpanReference
readW3CTraceHeader headers = do
  (_, headerValue) <- List.find (\(name, _) -> name == "traceparent") headers
  getTraceParent headerValue

getTraceParent :: T.Text -> Maybe SpanReference
getTraceParent headerValue = do
  let parts = T.split (== '-') headerValue
  if List.length parts == 4
    then pure $ SpanReference (TraceId $ parts !! 1) (SpanId $ parts !! 2)
    else Nothing

writeW3CTraceHeader :: SpanReference -> [(HeaderName, T.Text)]
writeW3CTraceHeader sr =
  [ ( "traceparent",
      "00-"
        <> coerce (getTraceId sr)
        <> "-"
        <> coerce (getSpanId sr)
        <> "-01"
    )
  ]

w3cTraceHeaderFormat :: TraceHeaderFormat
w3cTraceHeaderFormat =
  TraceHeaderFormat
    { parseTraceHeader = readW3CTraceHeader,
      writeTraceHeader = writeW3CTraceHeader
    }
