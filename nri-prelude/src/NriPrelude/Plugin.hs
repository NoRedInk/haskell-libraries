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
-- - Module providing API for creating plugins: https://www.stackage.org/haddock/lts-17.4/ghc-lib-8.10.4.20210206/GhcPlugins.html

import Data.Function ((&))
import qualified Data.List
import qualified GHC.Plugins as GhcPlugins
import qualified GHC.Hs
import qualified GHC
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

-- | adds an unqualified import of the NriPrelude module, and qualified imports of
-- other base modules such as List and Maybe.
--
-- To use it make sure your project has @nri-prelude@ listed as a dependency,
-- then add the follwing ghc option to your cabal or package yaml file:
--
-- > -fplugin=NriPrelude.Plugin
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
  GHC.Hs.HsParsedModule ->
  GhcPlugins.Hsc GHC.Hs.HsParsedModule
addImplicitImports _ _ parsed =
  Prelude.pure
    parsed
      { GHC.Hs.hpm_module =
          fmap addImportsWhenNotPath (GHC.Hs.hpm_module parsed)
      }
  where
    addImportsWhenNotPath hsModule =
      case fmap GHC.unLoc (hsmodName hsModule) of
        Nothing -> addImports hsModule
        Just modName ->
          if Data.List.isPrefixOf "Paths_" (show modName)
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
              case (isQualified imp, (show . GHC.unLoc . ideclName) imp) of
                (True, name) -> Qualified name
                (False, name) -> Unqualified name
          )
        & Set.fromList

    unqualified name =
      name &
      GhcPlugins.mkModuleName &
      simpleImportDecl &
      GHC.noLocA &
      fmap (\qual -> qual {ideclImplicit = True})
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
