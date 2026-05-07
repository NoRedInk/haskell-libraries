-- | Sync HTTP delivery of typed analytics events to NoRedInk's events
-- service. The `AnalyticsHandler` produced here is meant to be threaded
-- into `nri-prelude`'s `LogHandler` analytics callback at the
-- application root.
module Analytics
  ( -- * Handle
    Internal.AnalyticsHandler,
    sendEvent,
    handler,
    silentHandler,

    -- * Settings
    Internal.Settings
      ( Settings,
        eventsServiceUrl,
        timeoutMicros,
        authToken
      ),
    decoder,
  )
where

import qualified Analytics.Internal as Internal
import qualified Conduit
import qualified Data.Aeson as Aeson
import qualified Environment
import qualified Log
import qualified Network.HTTP.Client.TLS as HTTP.TLS
import NriPrelude
import Prelude (IO, pure)

-- | Send a single event payload. Called from
-- `Platform.Analytics.Internal.trackEvent` via the `LogHandler`'s
-- analytics callback. Synchronous; bounded by `Settings.timeoutMicros`;
-- never throws.
sendEvent :: Internal.AnalyticsHandler -> Aeson.Value -> IO ()
sendEvent = Internal.sendEvent

-- | A no-op handler. Use this in tests and on platforms that have not
-- opted in to analytics tracking yet.
silentHandler :: Internal.AnalyticsHandler
silentHandler =
  Internal.AnalyticsHandler
    { Internal.sendEvent = \_ -> pure (),
      Internal.settings =
        Internal.Settings
          { Internal.eventsServiceUrl = "",
            Internal.timeoutMicros = 0,
            Internal.authToken = Log.mkSecret ""
          },
      Internal.httpManager = Nothing
    }

-- | Acquire a live handler. Owns an HTTP manager that lives until the
-- `Acquire` is released.
handler :: Internal.Settings -> Conduit.Acquire Internal.AnalyticsHandler
handler s = do
  manager <- Conduit.mkAcquire HTTP.TLS.newTlsManager (\_ -> pure ())
  pure
    Internal.AnalyticsHandler
      { Internal.sendEvent = Internal.sendEventIO manager s,
        Internal.settings = s,
        Internal.httpManager = Just manager
      }

-- | Read settings from environment variables.
decoder :: Environment.Decoder Internal.Settings
decoder =
  pure Internal.Settings
    |> andMap eventsServiceUrlDecoder
    |> andMap timeoutMicrosDecoder
    |> andMap authTokenDecoder

eventsServiceUrlDecoder :: Environment.Decoder Text
eventsServiceUrlDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ANALYTICS_EVENTS_SERVICE_URL",
        Environment.description = "Base URL of the events service.",
        Environment.defaultValue = ""
      }
    Environment.text

timeoutMicrosDecoder :: Environment.Decoder Int
timeoutMicrosDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ANALYTICS_EVENTS_TIMEOUT_MICROS",
        Environment.description = "Per-request timeout in microseconds. Default 500000 (500ms).",
        Environment.defaultValue = "500000"
      }
    Environment.int

authTokenDecoder :: Environment.Decoder (Log.Secret Text)
authTokenDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "ANALYTICS_EVENTS_AUTH_TOKEN",
        Environment.description = "Bearer token for the events service.",
        Environment.defaultValue = ""
      }
    (Environment.secret Environment.text)
