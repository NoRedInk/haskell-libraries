let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc901.extend (self: super: {
    servant = super.servant_0_18_3;
    servant-server = super.servant-server_0_18_3;
    jose = pkgs.haskell.lib.dontCheck super.jose;
    servant-auth-server = pkgs.haskell.lib.dontCheck super.servant-auth-server;
  });
}
