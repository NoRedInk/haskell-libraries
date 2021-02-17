let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };

in pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc865
    pkgs.cabal-install
    pkgs.haskellPackages.hpack
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.ormolu
  ];
}
