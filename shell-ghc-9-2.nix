let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc926.extend (self: super:
    let
      packageFromSources = name: self.callCabal2nix name sources."${name}" { };
    in {
      # todo: resolve breaking changes in brick >= 0.72
      brick = self.callHackage "brick" "0.71.1" { };

      # tasty-test-reporter is marked as broken, get the same version we use for
      # ghc 9.4
      tasty-test-reporter = packageFromSources "tasty-test-reporter";

      hspec =
        pkgs.haskell.lib.doJailbreak (self.callHackage "hspec" "2.8.5" { });
      hspec-discover = self.callHackage "hspec-discover" "2.8.5" { };
      hspec-meta = self.callHackage "hspec-meta" "2.7.8" { };

      # servant-auth-server 0.4.8.0 is marked as broken in nixpkgs, so jailbreak
      # (known good) 0.4.7.0 to make it work
      servant-auth-server = pkgs.haskell.lib.doJailbreak
        (self.callHackage "servant-auth-server" "0.4.7.0" { });
      servant-auth = pkgs.haskell.lib.doJailbreak super.servant-auth;

      # required by servant-auth-server@0.4.7.0
      jose = pkgs.haskell.lib.dontCheck (self.callHackage "jose" "0.9" { });
      jose-jwt = self.callHackage "jose-jwt" "0.9.5" { };
    });
}
