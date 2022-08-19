let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8107.extend (self: super: {
    # Default is 1.1.0.0 but servant-auth-server requires >=1.2.1.0
    base64-bytestring = super.base64-bytestring_1_2_1_0;
    # Skip broken tests
    servant-auth-server = pkgs.haskell.lib.dontCheck super.servant-auth-server;
  });
}
