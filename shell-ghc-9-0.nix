let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs-unstable { };
  old_pkgs = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc902.extend (self: super: {
    # Tests are broken
    jose = pkgs.haskell.lib.dontCheck super.jose;
    # Default is 1.1.0.0 but servant-auth-server requires >=1.2.1.0
    base64-bytestring = super.base64-bytestring_1_2_1_0;
    # Default is 5.1.0 but nri-prelude and servant-server require <5.1.0
    lens = (super.callHackageDirect {
      pkg = "lens";
      ver = "5.0.1";
      sha256 = "sha256-mmH1AzFIFT04UFsqOInYaQFaVfzxgQ5D5k/fRpsYE8o=";
    }) {};
    # Skip broken tests
    servant-auth-server = pkgs.haskell.lib.dontCheck super.servant-auth-server;
    # Skip broken tests
    cryptonite = pkgs.haskell.lib.dontCheck super.cryptonite;
    # Skip broken tests
    http2 = pkgs.haskell.lib.dontCheck super.http2;
  });
}
