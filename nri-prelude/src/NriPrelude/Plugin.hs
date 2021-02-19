{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-- | A compiler plugin that imports the equivalents of modules that Elm will
-- also import.
--
--
-- Useful documentation
-- - Elm's default imports: https://package.elm-lang.org/packages/elm/core/latest/
-- - GHC user guide on compiler plugins: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html#compiler-plugins
-- - Module providing API for creating plugins: https://www.stackage.org/haddock/lts-17.4/ghc-lib-8.10.4.20210206/GhcPlugins.html
module NriPrelude.Plugin
  ( plugin,
  )
where

-- In GHC 8.10 and higher the imports below will come from the GHC.Hs module
-- instead. We'll need to handle this, maybe using some CPP-powered conditional
-- imports.

import Data.Function ((&))
import qualified Data.List
import qualified GhcPlugins
import NriPrelude.Plugin.GhcVersionDependent
  ( hsmodImports,
    hsmodName,
    ideclImplicit,
    ideclName,
    ideclQualified,
    isQualified,
    mkQualified,
    simpleImportDecl,
  )
import qualified Set
import Prelude

plugin :: GhcPlugins.Plugin
plugin =
  GhcPlugins.defaultPlugin
    { GhcPlugins.parsedResultAction = addImplicitImports,
      -- Let GHC know this plugin doesn't perform arbitrary IO. Given the same
      -- input file it will make the same changes. Without this GHC will
      -- recompile modules using this plugin every time which is expensive.
      GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
    }

addImplicitImports ::
  [GhcPlugins.CommandLineOption] ->
  GhcPlugins.ModSummary ->
  GhcPlugins.HsParsedModule ->
  GhcPlugins.Hsc GhcPlugins.HsParsedModule
addImplicitImports _ _ parsed =
  Prelude.pure
    parsed
      { GhcPlugins.hpm_module =
          fmap addImportsWhenNotPath (GhcPlugins.hpm_module parsed)
      }
  where
    addImportsWhenNotPath hsModule =
      case fmap unLocate (hsmodName hsModule) of
        Nothing -> addImports hsModule
        Just modName ->
          if Data.List.isPrefixOf "Paths_" modName
            then hsModule
            else addImports hsModule

    addImports hsModule =
      hsModule
        { hsmodImports =
            -- Add default Elm-like imports when the user hasn't imported them
            -- explicitly yet, in order to avoid duplicate import warnings.
            hsmodImports hsModule
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
      hsmodImports hsModule
        & fmap
          ( \(GhcPlugins.L _ imp) ->
              case (isQualified imp, unLocate (ideclName imp)) of
                (True, name) -> Qualified name
                (False, name) -> Unqualified name
          )
        & Set.fromList

    unLocate (GhcPlugins.L _ x) = GhcPlugins.moduleNameString x

    unqualified name =
      GhcPlugins.noLoc (simpleImportDecl (GhcPlugins.mkModuleName name))
        & fmap (\qual -> qual {ideclImplicit = True})
    qualified name =
      fmap (\qual -> qual {ideclQualified = mkQualified}) (unqualified name)

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
