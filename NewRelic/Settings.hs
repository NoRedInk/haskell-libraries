module Observability.NewRelic.Settings
  ( Settings (..),
    NewRelicStatusUrl (..),
    decoder,
  )
where

import Cherry.Prelude
import qualified Environment
import qualified Log
import qualified Tracing.NewRelic as NewRelic
import Prelude (pure)

data Settings
  = Settings
      { appName :: NewRelic.AppName,
        licenseKey :: Log.Secret NewRelic.LicenseKey,
        timeout :: NewRelic.TimeoutMs,
        newRelicStatusUrl :: NewRelicStatusUrl
      }

newtype NewRelicStatusUrl = NewRelicStatusUrl Text

decoder :: Environment.Decoder Settings
decoder =
  pure Settings
    |> andMap appNameDecoder
    |> andMap licenseKeyDecoder
    |> andMap timeoutDecoder
    |> andMap newRelicStatusUrlDecoder

appNameDecoder :: Environment.Decoder NewRelic.AppName
appNameDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_APP_NAME",
        Environment.description = "The NewRelic applicat name to connect to.",
        Environment.defaultValue = ""
      }
    (Environment.text |> map NewRelic.AppName)

licenseKeyDecoder :: Environment.Decoder (Log.Secret NewRelic.LicenseKey)
licenseKeyDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_LICENSE_KEY",
        Environment.description = "The NewRelic license key to connect with..",
        Environment.defaultValue = ""
      }
    (Environment.text |> map NewRelic.LicenseKey |> Environment.secret)

timeoutDecoder :: Environment.Decoder NewRelic.TimeoutMs
timeoutDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_TIMEOUT_MS",
        Environment.description = "The timeout when connecting to NewRelic",
        Environment.defaultValue = "10000"
      }
    (Environment.int |> map NewRelic.TimeoutMs)

newRelicStatusUrlDecoder :: Environment.Decoder NewRelicStatusUrl
newRelicStatusUrlDecoder =
  Environment.variable
    Environment.Variable
      { Environment.name = "NEW_RELIC_STATUS_URL",
        Environment.description = "This url is used to check the status of NewRelics.",
        Environment.defaultValue = "https://status.newrelic.com"
      }
    (Environment.text |> map NewRelicStatusUrl)
