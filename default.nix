let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    mkDerivation = expr:
      super.mkDerivation (expr // {
        enableLibraryProfiling = true;
        enableExecutableProfiling = true;
      });
    nri-prelude = self.callCabal2nix "nri-prelude" ./nri-prelude { };
    log-explorer = self.callCabal2nix "log-explorer" ./nri-log-explorer { };
    safe-coloured-text = self.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      self.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
  });

in {
  log-explorer =
    pkgs.haskell.lib.justStaticExecutables haskellPackages.log-explorer;
}
