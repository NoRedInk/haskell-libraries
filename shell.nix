{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = [ pkgs.ghc pkgs.cabal-install pkgs.haskellPackages.hpack ];
}
