{-# LANGUAGE FlexibleInstances #-}

module Honeycomb.Trace.Types.SpanContext
  ( SpanContext (..),
    spanReferenceL,
    parentSpanIdL,
    serviceNameL,
    spanNameL,
    spanEventL,
    inheritableFieldsL,
    HasSpanContext,
    spanContextL,
  )
where

import qualified Data.HashSet as HS
import qualified Data.Text as T
import Honeycomb.Core.Types.HoneyEvent
import Honeycomb.Trace.Types.ServiceName
import Honeycomb.Trace.Types.SpanId
import Honeycomb.Trace.Types.SpanName
import Honeycomb.Trace.Types.SpanReference
import Honeycomb.Trace.Types.TraceId
import Lens.Micro (Lens', lens)

data SpanContext = SpanContext
  { spanReference :: !SpanReference,
    parentSpanId :: !(Maybe SpanId),
    serviceName :: !ServiceName,
    spanName :: !SpanName,
    spanEvent :: !HoneyEvent,
    inheritableFields :: !(HS.HashSet T.Text)
  }
  deriving (Show)

spanReferenceL :: Lens' SpanContext SpanReference
spanReferenceL = lens spanReference (\x y -> x {spanReference = y})

parentSpanIdL :: Lens' SpanContext (Maybe SpanId)
parentSpanIdL = lens parentSpanId (\x y -> x {parentSpanId = y})

serviceNameL :: Lens' SpanContext ServiceName
serviceNameL = lens serviceName (\x y -> x {serviceName = y})

spanNameL :: Lens' SpanContext SpanName
spanNameL = lens spanName (\x y -> x {spanName = y})

spanEventL :: Lens' SpanContext HoneyEvent
spanEventL = lens spanEvent (\x y -> x {spanEvent = y})

inheritableFieldsL :: Lens' SpanContext (HS.HashSet T.Text)
inheritableFieldsL = lens inheritableFields (\x y -> x {inheritableFields = y})

class HasSpanContext env where
  spanContextL :: Lens' env (Maybe SpanContext)

instance HasSpanContext (Maybe SpanContext) where
  spanContextL = id

instance HasTraceId SpanContext where
  getTraceId = getTraceId . spanReference

instance HasSpanId SpanContext where
  getSpanId = getSpanId . spanReference
