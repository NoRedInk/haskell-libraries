{-# LANGUAGE FlexibleInstances #-}

-- | Internal entry point for emitting analytics events from `Task` code.
--
-- This module is intentionally `.Internal`. It is NOT re-exported from
-- the prelude's public `Platform` module. Higher layers (NoRedInk's
-- `Analytics.track`) wrap this and own the user-facing API; downstream
-- code that imports `Platform.Analytics.Internal` directly is bypassing
-- the closed event dictionary and should be flagged in review.
module Platform.Analytics.Internal
  ( trackEvent,
    AnalyticsEventDetails,
  )
where

import qualified Data.Aeson as Aeson
import NriPrelude
import qualified Platform
import qualified Platform.Internal as Internal
import Task (Task)
import qualified Prelude

-- | Send an analytics event. Opens a child tracing span named
-- @analytics.track@, attaches the JSON payload as the span's details,
-- and synchronously invokes the `LogHandler`'s analytics callback.
trackEvent :: (Aeson.ToJSON e) => e -> Task err ()
trackEvent event =
  let value = Aeson.toJSON event
   in Platform.tracingSpan "analytics.track" <| do
        Platform.setTracingSpanDetails (AnalyticsEventDetails value)
        Internal.Task
          ( \handler -> do
              Internal.trackAnalyticsEventIO handler value
              Prelude.pure (Ok ())
          )

-- | A `TracingSpanDetails` wrapper around the analytics event payload, so
-- that the JSON we send to the analytics backend is also attached to the
-- @analytics.track@ span and visible in the existing observability
-- reporters.
newtype AnalyticsEventDetails = AnalyticsEventDetails Aeson.Value
  deriving (Aeson.ToJSON)

instance Internal.TracingSpanDetails AnalyticsEventDetails
