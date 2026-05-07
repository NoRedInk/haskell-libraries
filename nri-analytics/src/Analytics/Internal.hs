module Analytics.Internal
  ( AnalyticsHandler (..),
    Settings (..),
    buildRequest,
    sendEventIO,
  )
where

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text
import qualified Data.Text.Encoding
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
    -- | Bearer token used in the Authorization header.
    authToken :: Text
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
            ("Authorization", "Bearer " ++ Data.Text.Encoding.encodeUtf8 (authToken s))
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
    request <- buildRequest s value
    _response <- HTTP.httpLbs request manager
    pure ()
  where
    logAndDrop :: Exception.SomeException -> IO ()
    logAndDrop _e = pure ()
