{-# LANGUAGE CPP #-}

-- Imports we need to write this module tend to move around in later
-- versions of GHC.  This module uses the CPP extension to import the
-- right values dependent on the version of GHC.

module NriPrelude.Plugin.GhcVersionDependent (
  setIDeclImplicit,
  withParsedResult
) where

import qualified GHC.Hs
import qualified GHC.Hs.ImpExp 
import Prelude

#if __GLASGOW_HASKELL__ >= 904
import qualified GHC.Driver.Plugins
#endif

#if __GLASGOW_HASKELL__ >= 904
withParsedResult :: GHC.Driver.Plugins.ParsedResult -> (GHC.Hs.HsParsedModule -> GHC.Hs.HsParsedModule) -> GHC.Driver.Plugins.ParsedResult
withParsedResult parsed f =
  parsed
    { GHC.Driver.Plugins.parsedResultModule = f (GHC.Driver.Plugins.parsedResultModule parsed)
    }
#else
withParsedResult :: GHC.Hs.HsParsedModule -> (GHC.Hs.HsParsedModule -> GHC.Hs.HsParsedModule) -> GHC.Hs.HsParsedModule
withParsedResult parsed f = f parsed
#endif

#if __GLASGOW_HASKELL__ >= 906
setIDeclImplicit :: Bool -> GHC.Hs.ImpExp.ImportDecl GHC.Hs.GhcPs -> GHC.Hs.ImpExp.ImportDecl GHC.Hs.GhcPs
setIDeclImplicit isImplicit importDecl =
  -- no idea what `XImportDeclPass` _is_, btw.  just following types
  -- ref: https://hackage.haskell.org/package/ghc-9.6.5/docs/Language-Haskell-Syntax-ImpExp.html#t:ImportDecl
  --      https://hackage.haskell.org/package/ghc-9.6.5/docs/Language-Haskell-Syntax-Extension.html#t:XCImportDecl
  --      https://hackage.haskell.org/package/ghc-9.6.5/docs/GHC-Hs-ImpExp.html#t:XImportDeclPass
  let xImportDeclPass = GHC.Hs.ImpExp.ideclExt importDecl
   in importDecl
        { GHC.Hs.ImpExp.ideclExt =
            xImportDeclPass {GHC.Hs.ImpExp.ideclImplicit = isImplicit}
        }
#else
setIDeclImplicit :: Bool -> GHC.Hs.ImpExp.ImportDecl GHC.Hs.GhcPs -> GHC.Hs.ImpExp.ImportDecl GHC.Hs.GhcPs
setIDeclImplicit isImplicit importDecl = importDecl {GHC.Hs.ImpExp.ideclImplicit = isImplicit}
#endif
