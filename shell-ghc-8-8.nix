let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-20-09 { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc884;
}
