-- | Readiness check for tracing using NewRelic
-- We check two things:
-- * the NewRelic status page
-- * the daemon that is used to send data to NewRelic
module Observability.NewRelic.Health
  ( readiness,
  )
where

import qualified Char
import Cherry.Prelude
import qualified Conduit
import Control.Concurrent.Async as Async
import Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import qualified Health
import qualified Http
import qualified Platform
import Servant.API ((:>), Get, JSON)
import qualified Servant.Client as Client
import qualified Observability.NewRelic.Settings as Settings
import Observability.NewRelic.Settings (Settings)
import qualified Tracing.NewRelic as NewRelic
import Prelude (Either (Left, Right), IO, pure)

readiness :: NewRelic.App -> Settings -> Health.Check
readiness app settings =
  Health.mkCheck "newrelic"
    <| Async.runConcurrently
    <| Async.Concurrently (daemonReady app)
    ++ Async.Concurrently (newRelicStatus (Settings.newRelicStatusUrl settings))

-- | Check if the daemon is ready.
-- There is no function to ping the daemon or check if it's ready.  We create a
-- transaction, ignore and eventually ending it.  This does utilize the daemon
-- and talks to NewRelic without actually creating any data on NewRelic.
daemonReady :: NewRelic.App -> IO Health.Status
daemonReady app = do
  maybeTransaction <- NewRelic.startWebTransaction app "NewRelic health check."
  case maybeTransaction of
    Nothing -> pure (bad "Couldn't create transaction.")
    Just transaction -> do
      ignored <- NewRelic.ignoreTransaction transaction
      let ignoredStatus =
            if ignored
              then Health.Good
              else bad "Couldn't ignore transaction."
      ended <- NewRelic.endTransaction transaction
      let endedStatus =
            if ended
              then Health.Good
              else bad "Couldn't end transaction."
      pure (ignoredStatus ++ endedStatus)

-- | Check if NewRelics is up and running.
-- See https://status.newrelic.com/api for more informations.
-- The url for requestiong the status.json is configured in NEW_RELIC_STATUS_URL.
newRelicStatus :: Settings.NewRelicStatusUrl -> IO Health.Status
newRelicStatus (Settings.NewRelicStatusUrl url) =
  Conduit.withAcquire Http.handler <| \handler -> do
    baseUrl <- Client.parseBaseUrl (Data.Text.unpack url)
    log <- Platform.silentHandler
    result <-
      Http.withThirdPartyIO
        log
        handler
        ( \manager ->
            let env = Client.mkClientEnv manager baseUrl
                client = Client.client (Proxy :: Proxy API)
             in Client.runClientM client env
        )
    case result of
      Right NewRelicsStatus {status} ->
        pure (toHealthStatus status)
      Left x ->
        Exception.displayException x
          |> Data.Text.pack
          |> bad
          |> pure

toHealthStatus :: Status -> Health.Status
toHealthStatus Status {indicator, description} =
  case indicator of
    None -> Health.Good
    _ -> bad description

-- NewRelics Status

-- | The endpoint to get the status json.
type API =
  "api"
    :> "v2"
    :> "status.json"
    :> Get '[JSON] NewRelicsStatus

newtype NewRelicsStatus
  = NewRelicsStatus
      {status :: Status}
  deriving (Generic)

data Status
  = Status
      { indicator :: Indicator,
        description :: Text
      }
  deriving (Generic)

data Indicator
  = None
  | Minor
  | Major
  | Critical
  deriving (Generic)

--  JSON instances

instance Aeson.FromJSON NewRelicsStatus

instance Aeson.FromJSON Status

instance Aeson.FromJSON Indicator where
  parseJSON =
    Aeson.genericParseJSON
      <| Aeson.defaultOptions
        { Aeson.constructorTagModifier = map Char.toLower
        }

bad :: Text -> Health.Status
bad msg = Health.Bad ("Observability.NewRelic.Health: " ++ msg)
