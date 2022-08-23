let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
  pkgs-unstable = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  pkgs-unstable = pkgs-unstable;
  haskellPackages = pkgs.haskell.packages.ghc924.extend (self: super: {
    jose = pkgs.haskell.lib.dontCheck super.jose;
    servant-auth-server = pkgs.haskell.lib.dontCheck super.servant-auth-server;
  });
}
