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
import qualified Environment
import qualified Health
import qualified Log
import qualified Network.Bugsnag as Bugsnag
import qualified Network.HTTP.Client
import qualified Platform
import qualified Prelude

logger :: Platform.Span -> Prelude.IO ()
logger = Prelude.undefined

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
