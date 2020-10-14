-- | A simple Redis library providing high level access to Redis features we
-- use here at NoRedInk
--
-- As with our Ruby Redis access, we enforce working within a "namespace".
module Redis
  ( -- Settings
    Settings.Settings (..),
    Settings.decoder,
    -- Internal
    Internal.Error (..),
    Internal.Handler,
    Internal.addNamespace,
    -- Real
    Real.Info (..),
    Real.handler,
    readiness,
  )
where

import qualified Health
import Nri.Prelude
import qualified Platform
import qualified Redis.Internal as Internal
import qualified Redis.Real as Real
import qualified Redis.Settings as Settings
import qualified Task

-- |
-- Check that we are ready to be take traffic.
readiness :: Internal.Handler -> Health.Check
readiness handler =
  Health.mkCheck "redis" <| do
    log <- Platform.silentHandler
    Internal.Ping
      |> Internal.query handler
      |> Task.map (\_ -> Health.Good)
      |> Task.onError (\err -> Task.succeed (Health.Bad (Internal.errorForHumans err)))
      |> Task.perform log
