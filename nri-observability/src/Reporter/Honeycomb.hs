-- | This reporter logs execution to https://honeycomb.io.
module Reporter.Honeycomb
  ( Internal.report,
    Internal.handler,
    Internal.Handler,
    Internal.Settings (..),
    Internal.decoder,
  )
where

import qualified Reporter.Honeycomb.Internal as Internal
