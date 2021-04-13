let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    nri-prelude = super.callCabal2nix "nri-prelude" ./nri-prelude { };
    log-explorer = super.callCabal2nix "log-explorer" ./nri-log-explorer { };
    pretty-diff = super.callCabal2nix "pretty-diff" sources.pretty-diff { };
    safe-coloured-text =
      super.callCabal2nix "safe-coloured-text" "${sources.safe-coloured-text}/safe-coloured-text" { };
  });

in {
  log-explorer =
    pkgs.haskell.lib.justStaticExecutables haskellPackages.log-explorer;
}
