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
import qualified Data.Aeson.KeyMap as KeyMap
import NriPrelude
import qualified Platform
import qualified Platform.Internal as Internal
import Task (Task)
import qualified Prelude

-- | Send an analytics event. Opens a child tracing span named
-- @analytics.track@, stamps the current request's session id (if any)
-- onto the event, attaches the resulting JSON payload as the span's
-- details, and synchronously invokes the `LogHandler`'s analytics
-- callback.
trackEvent :: (Aeson.ToJSON e) => e -> Task err ()
trackEvent event =
  Platform.tracingSpan "analytics.track" <| do
    mSid <- Platform.sessionId
    let value = stampSessionId mSid (Aeson.toJSON event)
    Platform.setTracingSpanDetails (AnalyticsEventDetails value)
    Internal.Task
      ( \handler -> do
          Internal.trackAnalyticsEventIO handler value
          Prelude.pure (Ok ())
      )

-- | Shallow-merge `session_id` onto an event payload. We only stamp
-- when the inbound value is a JSON object — every Event sum-type
-- variant serializes to an object, so non-object inputs are
-- defensively left untouched. Existing `session_id` keys on the body
-- are overwritten so the request-scoped id always wins.
stampSessionId :: Maybe Text -> Aeson.Value -> Aeson.Value
stampSessionId Nothing v = v
stampSessionId (Just sid) v = case v of
  Aeson.Object body ->
    Aeson.Object (KeyMap.insert "session_id" (Aeson.String sid) body)
  other -> other

-- | A `TracingSpanDetails` wrapper around the analytics event payload, so
-- that the JSON we send to the analytics backend is also attached to the
-- @analytics.track@ span and visible in the existing observability
-- reporters.
newtype AnalyticsEventDetails = AnalyticsEventDetails Aeson.Value
  deriving (Aeson.ToJSON)

instance Internal.TracingSpanDetails AnalyticsEventDetails
