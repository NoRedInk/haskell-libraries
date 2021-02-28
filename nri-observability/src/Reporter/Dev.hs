-- | Reporting for development
--
-- This reporter logs basic information about requests in a human-readable
-- format, for use in a development console.
--
-- The development logs produced are quite sparse. To dig deeper this reporter
-- will also make tracing data available in the log-explorer tool. You can find
-- it here:
--
-- <https://github.com/NoRedInk/haskell-libraries/tree/trunk/nri-log-explorer>
module Reporter.Dev
  ( Internal.report,
    Internal.Handler,
    Internal.handler,
    Internal.cleanup,
  )
where

import qualified Reporter.Dev.Internal as Internal
