module Observability.Bugsnag
  ( logger,
    Settings,
    decoder,
    readiness,
  )
where

import Cherry.Prelude
import qualified Control.Exception.Safe as Exception
import qualified Data.Text
import qualified Debug
import qualified Environment
import qualified Health
import qualified Http
import qualified Log
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client
import qualified Platform
import qualified Prelude

logger :: Http.Handler -> Settings -> Platform.Span -> Prelude.IO ()
logger http settings span = do
  let send' = send http settings
  case Platform.succeeded span of
    Platform.Succeeded -> Prelude.pure ()
    Platform.Failed -> send' (toEvent Nothing span)
    Platform.FailedWith err -> send' (toEvent (Just err) span)

send :: Http.Handler -> Settings -> Bugsnag.Event -> Prelude.IO ()
send http settings event = do
  log <- Platform.silentHandler
  Http.withThirdPartyIO log http <| \manager -> do
    -- Logging to Bugsnag might fail, but if it does we can't very well send the
    -- error to Bugsnag. This is the end of the line, these errors disappear
    -- into the aether.
    _ <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) [event]
    Prelude.pure ()

toEvent :: Maybe Exception.SomeException -> Platform.Span -> Bugsnag.Event
toEvent _ _ =
  Bugsnag.defaultEvent
    { Bugsnag.event_exceptions = [Debug.todo "Bugsnag.Exception"],
      Bugsnag.event_breadcrumbs = Just [Debug.todo "Bugsnag.Breadcrumb"],
      Bugsnag.event_request = Just (Debug.todo "Bugsnag.Request"),
      -- Bugsnag supports sending information about the thread we're running on,
      -- but this seems catered to heavyweight threads, surviving multiple
      -- requests. That's not Haskell and so we keep this empty.
      Bugsnag.event_threads = Nothing,
      Bugsnag.event_context = Just (Debug.todo "Text: what's happening"),
      Bugsnag.event_groupingHash = Just (Debug.todo "Text: override bugsnag's grouping logic"),
      Bugsnag.event_severity = Just (Debug.todo "Bugsnag.Severity"),
      Bugsnag.event_severityReason = Just (Debug.todo "Bugsnag.SeverityReason"),
      Bugsnag.event_user = Just (Debug.todo "Bugsnag.User"),
      Bugsnag.event_app = Just (Debug.todo "Bugsnag.App"),
      Bugsnag.event_device = Just (Debug.todo "Bugsnag.Device"),
      Bugsnag.event_session = Just (Debug.todo "Bugsnag.Session"),
      Bugsnag.event_metaData = Just (Debug.todo "Data.Aeson.Object")
    }

newtype Settings
  = Settings
      { apiKey :: Log.Secret Bugsnag.ApiKey
      }

decoder :: Environment.Decoder Settings
decoder =
  Prelude.pure Settings
    |> andMap apiKeyDecoder

apiKeyDecoder :: Environment.Decoder (Log.Secret Bugsnag.ApiKey)
apiKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "BUGSNAG_API_KEY",
        Environment.description = "The API key of the Bugsnag project we should send items too.",
        Environment.defaultValue = "*****"
      }
    (Environment.text |> map Bugsnag.apiKey |> Environment.secret)

-- |
-- Check if Bugsnag is ready to receive requests.
readiness :: Settings -> Network.HTTP.Client.Manager -> Health.Check
readiness settings manager =
  Health.mkCheck "bugsnag" <| do
    result <- Bugsnag.sendEvents manager (Log.unSecret (apiKey settings)) []
    Prelude.pure <| case result of
      Prelude.Right () -> Health.Good
      Prelude.Left err ->
        "HTTP request to Bugsnag failed: " ++ Exception.displayException err
          |> Data.Text.pack
          |> Health.Bad

_getRevision :: Prelude.IO Text
_getRevision = do
  eitherRevision <- Exception.tryAny <| Prelude.readFile "revision"
  case eitherRevision of
    Prelude.Left _err ->
      Prelude.pure "no revision file found"
    Prelude.Right version -> Prelude.pure <| Data.Text.pack version
