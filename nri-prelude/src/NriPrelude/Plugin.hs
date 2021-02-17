{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-- | A compiler plugin that imports the equivalents of modules that Elm will
-- also import.
--
-- Useful documentation
-- - GHC user guide on compiler plugins: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html#compiler-plugins
-- - Module providing API for creating plugins: https://www.stackage.org/haddock/lts-17.4/ghc-lib-8.10.4.20210206/GhcPlugins.html
module NriPrelude.Plugin
  ( plugin,
  )
where

import qualified GhcPlugins
-- In GHC 8.10 and higher the imports below will come from the GHC.Hs module
-- instead. We'll need to handle this, maybe using some CPP-powered conditional
-- imports.
import HsSyn (hsmodImports, ideclQualified, simpleImportDecl)
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
          fmap addImports (GhcPlugins.hpm_module parsed)
      }
  where
    addImports hsModule =
      hsModule {hsmodImports = hsmodImports hsModule ++ extraImports}
    extraImports =
      [ unqualified "NriPrelude",
        qualified "Debug"
      ]
    unqualified name =
      GhcPlugins.noLoc (simpleImportDecl (GhcPlugins.mkModuleName name))
    qualified name =
      fmap (\qual -> qual {ideclQualified = True}) (unqualified name)
