let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc901.extend (self: super: {
    jose = pkgs.haskell.lib.dontCheck super.jose;
    servant-auth-server = pkgs.haskell.lib.dontCheck super.servant-auth-server;
  });
}
