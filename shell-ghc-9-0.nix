let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc901;
  use_servant_0_18_3 = true;
}
