-- | Honeycomb
--
-- This reporter logs execution to https://honeycomb.io.
--
-- It does some custom stuff compared to other reporters:
--
-- * Sample requests based on
--   * Response type
--   * Endpoint (log fewer healthchecks)
-- * Calculates statistics over child spans per type
-- * Enriches child spans with data to help track problems
--   * Server (Pod, in k8s' case) hostname
--   * Http endpoint
module Reporter.Honeycomb
  ( Internal.report,
    Internal.handler,
    Internal.Handler,
    Internal.Settings,
    Internal.decoder,
  )
where

import qualified Reporter.Honeycomb.Internal as Internal
