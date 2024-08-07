{-# LANGUAGE CPP #-}

-- Imports we need to write this module tend to move around in later
-- versions of GHC.  This module uses the CPP extension to import the
-- right values dependent on the version of GHC.

module NriPrelude.Plugin.GhcVersionDependent (
  withParsedResult
) where

import GHC.Hs (HsParsedModule)

#if __GLASGOW_HASKELL__ >= 904
import qualified GHC.Driver.Plugins
#endif

#if __GLASGOW_HASKELL__ >= 904
withParsedResult :: GHC.Driver.Plugins.ParsedResult -> (HsParsedModule -> HsParsedModule) -> GHC.Driver.Plugins.ParsedResult
withParsedResult parsed f =
  parsed
    { GHC.Driver.Plugins.parsedResultModule = f (GHC.Driver.Plugins.parsedResultModule parsed)
    }
#else
withParsedResult :: HsParsedModule -> (HsParsedModule -> HsParsedModule) -> HsParsedModule
withParsedResult parsed f = f parsed
#endif
