let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-21-05 { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8104;
}
