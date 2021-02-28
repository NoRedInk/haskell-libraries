-- | Reporting to a file.
--
-- This reporter logs debugging information about completed requests to a file
-- or `stdout` (in that case, pass in `/dev/stdout` as the file to log to).
--
-- Every line this reporter logs is a JSON string. This 'structured logging'
-- output is optimized for external logging platforms that display these logs in
-- a pretty UI.
--
-- This logger supports sampling of successful requests, to help save money when
-- sending logs to external services.
module Reporter.File
  ( Internal.report,
    Internal.Handler,
    Internal.handler,
    Internal.cleanup,
    Internal.Settings,
    Internal.decoder,
  )
where

import qualified Reporter.File.Internal as Internal
