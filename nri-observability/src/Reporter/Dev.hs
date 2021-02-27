-- | Reporting for development
--
-- This reporter logs information about requests in a human-readable format, for
-- use in development.
module Reporter.Dev
  ( Internal.report,
    Internal.Handler,
    Internal.handler,
    Internal.cleanup,
    Internal.Settings,
    Internal.decoder,
  )
where

import qualified Reporter.Dev.Internal as Internal
