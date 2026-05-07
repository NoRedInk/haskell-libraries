module Analytics.Internal
  ( AnalyticsHandler (..),
    Settings (..),
    buildRequest,
    sendEventIO,
    stampEnvelope,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.UUID
import qualified Data.UUID.V4 as UUID
import qualified Log
import qualified Network.HTTP.Client as HTTP
import NriPrelude
import qualified Prelude
import Prelude (IO, pure)

-- | Configuration for the analytics handler. Loaded from environment
-- variables via `Analytics.decoder`. The events service contract is
-- provisional; revise these fields as the contract solidifies.
data Settings = Settings
  { -- | Base URL of the events service, e.g. https://events.noredink.com.
    eventsServiceUrl :: Text,
    -- | Per-request HTTP timeout. The request thread blocks for at most
    -- this long before the event is dropped.
    timeoutMicros :: Int,
    -- | Bearer token used in the Authorization header. Wrapped in
    -- `Log.Secret` so accidental logging or `Show`-deriving doesn't
    -- leak it.
    authToken :: Log.Secret Text
  }

-- | Live handle for delivering events. The `sendEvent` callback is what
-- gets threaded into nri-prelude's `LogHandler`. The other fields are
-- internal but must be retained to keep the HTTP manager + counters
-- alive for the program's lifetime.
data AnalyticsHandler = AnalyticsHandler
  { sendEvent :: Aeson.Value -> IO (),
    settings :: Settings,
    httpManager :: Maybe HTTP.Manager
  }

-- | Build the HTTP request for a single event. Pure-ish: doesn't fire the
-- request, just constructs it. Tested directly so we don't have to spin
-- up a server.
buildRequest :: Settings -> Aeson.Value -> IO HTTP.Request
buildRequest s value = do
  initial <- HTTP.parseRequest (Data.Text.unpack (eventsServiceUrl s ++ "/events"))
  pure
    initial
      { HTTP.method = "POST",
        HTTP.requestHeaders =
          [ ("Content-Type", "application/json"),
            ("Authorization", "Bearer " ++ Data.Text.Encoding.encodeUtf8 (Log.unSecret (authToken s)))
          ],
        HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode value),
        HTTP.responseTimeout = HTTP.responseTimeoutMicro (Prelude.fromIntegral (timeoutMicros s))
      }

-- | Synchronous HTTP delivery. Catches every exception and drops the
-- event so analytics failure can never bring down the surrounding
-- request. (Logging hook is a TODO until the prelude logger is wired
-- through here.)
sendEventIO :: HTTP.Manager -> Settings -> Aeson.Value -> IO ()
sendEventIO manager s value =
  Exception.handleAny logAndDrop <| do
    enveloped <- stampEnvelope value
    request <- buildRequest s enveloped
    _response <- HTTP.httpLbs request manager
    pure ()
  where
    logAndDrop :: Exception.SomeException -> IO ()
    logAndDrop _e = pure ()

-- | Mint event_id (UUID v4) + event_timestamp (ISO 8601 UTC) and
-- shallow-merge them onto the event JSON. Envelope keys win on
-- collision. Non-object inbound values are wrapped under a `payload`
-- key so the envelope still produces a valid object.
stampEnvelope :: Aeson.Value -> IO Aeson.Value
stampEnvelope inbound = do
  uuid <- UUID.nextRandom
  now <- Clock.getCurrentTime
  let envelope =
        KeyMap.fromList
          [ ("event_id", Aeson.String (Data.UUID.toText uuid)),
            ("event_timestamp", Aeson.String (Data.Text.pack (ISO8601.iso8601Show now)))
          ]
  pure <| case inbound of
    Aeson.Object body -> Aeson.Object (KeyMap.union envelope body)
    other -> Aeson.Object (KeyMap.insert "payload" other envelope)
