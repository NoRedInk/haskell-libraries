{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-- | A GHC plugin for a more Elm-like Haskell experience. It automatically
-- adds an unqualified import of the NriPrelude module, and qualified imports of
-- other base modules such as List and Maybe.
--
-- To use it make sure your project has @nri-prelude@ listed as a dependency,
-- then add the follwing ghc option to your cabal or package yaml file:
--
-- > -fplugin=NriPrelude.Plugin
module NriPrelude.Plugin
  ( plugin,
  )
where

-- Useful documentation
-- - Elm's default imports: https://package.elm-lang.org/packages/elm/core/latest/
-- - GHC user guide on compiler plugins: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html#compiler-plugins
-- - Module providing API for creating plugins: https://hackage.haskell.org/package/ghc-lib-9.6.5.20240423/docs/GHC-Plugins.html

import Data.Function ((&))
import qualified Data.List

import qualified GHC.Hs
import qualified GHC.Parser.Annotation
import qualified GHC.Plugins

import NriPrelude.Plugin.GhcVersionDependent (setIDeclImplicit, withParsedResult)
import qualified Set
import Prelude

-- | adds an unqualified import of the NriPrelude module, and qualified imports of
-- other base modules such as List and Maybe.
--
-- To use it make sure your project has @nri-prelude@ listed as a dependency,
-- then add the follwing ghc option to your cabal or package yaml file:
--
-- > -fplugin=NriPrelude.Plugin
plugin :: GHC.Plugins.Plugin
plugin =
  GHC.Plugins.defaultPlugin
    { GHC.Plugins.parsedResultAction = addImplicitImports,
      -- Let GHC know this plugin doesn't perform arbitrary IO. Given the same
      -- input file it will make the same changes. Without this GHC will
      -- recompile modules using this plugin every time which is expensive.
      GHC.Plugins.pluginRecompile = GHC.Plugins.purePlugin
    }

addImplicitImports ::
  [GHC.Plugins.CommandLineOption] ->
  GHC.Plugins.ModSummary ->
#if __GLASGOW_HASKELL__ >= 904
  GHC.Plugins.ParsedResult ->
  GHC.Plugins.Hsc GHC.Plugins.ParsedResult
#else
  GHC.Hs.HsParsedModule ->
  GHC.Plugins.Hsc GHC.Hs.HsParsedModule
#endif
addImplicitImports _ _ parsed =
  Prelude.pure $
    withParsedResult parsed $ \parsed' ->
      parsed'
        { GHC.Hs.hpm_module =
            fmap addImportsWhenNotPath (GHC.Hs.hpm_module parsed')
        }
  where
    addImportsWhenNotPath hsModule =
      case fmap unLocate (GHC.Hs.hsmodName hsModule) of
        Nothing -> addImports hsModule
        Just modName ->
          if Data.List.isPrefixOf "Paths_" modName
            then hsModule
            else addImports hsModule

    addImports hsModule =
      hsModule
        { GHC.Hs.hsmodImports =
            -- Add default Elm-like imports when the user hasn't imported them
            -- explicitly yet, in order to avoid duplicate import warnings.
            GHC.Hs.hsmodImports hsModule
              ++ ( Set.diff extraImports (existingImports hsModule)
                     & Set.toList
                     & fmap
                       ( \imp ->
                           case imp of
                             Unqualified name -> unqualified name
                             Qualified name -> qualified name
                       )
                 )
        }

    existingImports hsModule =
      GHC.Hs.hsmodImports hsModule
        & fmap
          ( \(GHC.Plugins.L _ imp) ->
              case (isQualified imp, unLocate (GHC.Hs.ideclName imp)) of
                (True, name) -> Qualified name
                (False, name) -> Unqualified name
          )
        & Set.fromList

    unLocate (GHC.Plugins.L _ x) = GHC.Plugins.moduleNameString x

    unqualified name =
      GHC.Parser.Annotation.noLocA (GHC.Hs.simpleImportDecl (GHC.Plugins.mkModuleName name))
        & fmap (setIDeclImplicit True)
    qualified name =
      fmap (\qual -> qual {GHC.Hs.ideclQualified = GHC.Hs.QualifiedPre}) (unqualified name)

-- There's more than one way to do a qualified import. See:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/import_qualified_post.html

isQualified :: GHC.Hs.ImportDecl pass -> Bool
isQualified imp =
  case GHC.Hs.ideclQualified imp of
    GHC.Hs.QualifiedPre -> True
    GHC.Hs.QualifiedPost -> True
    GHC.Hs.NotQualified -> False

data Import
  = Unqualified String
  | Qualified String
  deriving (Eq, Ord)

-- taken from https://package.elm-lang.org/packages/elm/core/latest/
extraImports :: Set.Set Import
extraImports =
  Set.fromList
    [ Unqualified "NriPrelude", -- Elm exports types from withi these modules. We re-export them from NriPrelude. Same effect.
      Qualified "Basics",
      Qualified "Char",
      Qualified "Debug",
      Qualified "List",
      Qualified "Maybe",
      Qualified "Platform",
      Qualified "Result",
      Qualified "Text", -- equivalent to Elm's String
      Qualified "Tuple",
      -- Additionally Task and Log because we use them everywhere
      Qualified "Log",
      Qualified "Task"
    ]
