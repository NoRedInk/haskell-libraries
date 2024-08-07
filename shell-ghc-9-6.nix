let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  commonHaskellOverrides = import ./nix/common-haskell-overrides.nix { inherit sources pkgs; };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc965.extend commonHaskellOverrides;
}
