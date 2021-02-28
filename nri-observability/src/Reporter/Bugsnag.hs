-- | This reporter sends tracing information for failing requests to Bugsnag,
-- a platform for monitoring applications. Learn more about Bugsnag on their
-- website:
--
-- <https://www.bugsnag.com/>
module Reporter.Bugsnag
  ( Internal.report,
    Internal.Handler,
    Internal.handler,
    Internal.Settings,
    Internal.decoder,
  )
where

import qualified Reporter.Bugsnag.Internal as Internal
