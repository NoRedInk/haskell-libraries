{-# LANGUAGE CPP #-}

-- A couple of imports we need to write this module have been moved in GHC
-- version 8.10. This module uses the CPP extension to import the right values
-- dependent on the version of GHC.

#if __GLASGOW_HASKELL__ >= 810

module NriPrelude.Plugin.GhcVersionDependent (
  module GHC.Hs,
  isQualified,
  mkQualified,
  noLoc,
  withParsedResult
) where

import GHC.Hs
import Prelude

#if __GLASGOW_HASKELL__ >= 904
import qualified GHC.Driver.Plugins
#endif
#if __GLASGOW_HASKELL__ >= 902
import qualified GHC.Parser.Annotation
#elif __GLASGOW_HASKELL__ >= 900
import GHC.Types.SrcLoc (noLoc)
#else
import GhcPlugins (HsParsedModule)
import SrcLoc (noLoc)
#endif

#if __GLASGOW_HASKELL__ >= 902
noLoc :: a -> GHC.Parser.Annotation.LocatedAn an a
noLoc = noLocA
#endif

-- There's more than one way to do a qualified import. See:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/import_qualified_post.html

isQualified :: ImportDecl pass -> Bool
isQualified imp =
  case ideclQualified imp of
    QualifiedPre -> True
    QualifiedPost -> True
    NotQualified -> False

mkQualified :: ImportDeclQualifiedStyle
mkQualified = QualifiedPre

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

#else
module NriPrelude.Plugin.GhcVersionDependent (
  module HsSyn,
  isQualified,
  mkQualified,
  noLoc,
  withParsedResult
) where

import GhcPlugins (HsParsedModule, noLoc)
import HsSyn
import Prelude

isQualified :: ImportDecl pass -> Bool
isQualified = ideclQualified

mkQualified :: Bool
mkQualified = True

withParsedResult :: HsParsedModule -> (HsParsedModule -> HsParsedModule) -> HsParsedModule
withParsedResult parsed f = f parsed

#endif
