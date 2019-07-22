module Honeycomb.HTTP.Client where

import Data.Coerce (coerce)
import Honeycomb.Trace
import Data.Aeson (FromJSON (..), Value)
import RIO

import qualified Network.HTTP.Simple as H
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as LBS

{-
// VERSION;PAYLOAD

// VERSION=1
// =========
// PAYLOAD is a list of comma-separated params (k=v pairs), with no spaces.  recognized
// keys + value types:
//
//  trace_id=${traceId}     - traceId is an opaque ascii string which shall not include ','
//  parent_id=${spanId}     - spanId is an opaque ascii string which shall not include ','
//  dataset=${datasetId}   - datasetId is the slug for the honeycomb dataset to which downstream spans should be sent; shall not include ','
//  context=${contextBlob}  - contextBlob is a base64 encoded json object.
//
// ex: X-Honeycomb-Trace: 1;trace_id=weofijwoeifj,parent_id=owefjoweifj,context=SGVsbG8gV29ybGQ=
-}

addTraceHeader :: SpanContext -> H.Request -> H.Request
addTraceHeader (SpanContext { spanReference }) =
  H.addRequestHeader
    "X-Honeycomb-Trace"
    (headerValue (getTraceId spanReference) (getSpanId spanReference))
 where
  headerValue :: TraceId -> SpanId -> BS.ByteString
  headerValue tid sid =
    "1;trace_id=" <> (encodeUtf8 $ coerce tid) <> ",span_id=" <> (encodeUtf8 $ coerce sid)

addTraceHeaderM :: (MonadReader env m, HasSpanContext env) => H.Request -> m (H.Request)
addTraceHeaderM req = do
  sc <- view spanContextL
  pure $ maybe req (\s -> addTraceHeader s req) sc

httpBS :: (MonadIO m, MonadReader env m, HasSpanContext env) => H.Request -> m (H.Response BS.ByteString)
httpBS = addTraceHeaderM >=> H.httpBS

httpLBS :: (MonadIO m, MonadReader env m, HasSpanContext env) => H.Request -> m (H.Response LBS.ByteString)
httpLBS = addTraceHeaderM >=> H.httpLBS

httpNoBody :: (MonadIO m, MonadReader env m, HasSpanContext env) => H.Request -> m (H.Response ())
httpNoBody = addTraceHeaderM >=> H.httpNoBody

httpJSON :: (MonadIO m, MonadReader env m, HasSpanContext env, FromJSON a) => H.Request -> m (H.Response a)
httpJSON = addTraceHeaderM >=> H.httpJSON