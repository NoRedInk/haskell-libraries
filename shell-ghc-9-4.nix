let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc947.extend (self: super:
    let
      packageFromSources = name: self.callCabal2nix name sources."${name}" { };
    in {
      # todo: resolve breaking changes in brick >= 0.72, also jailbreak
      # to allow ghc 9.4.x
      brick =
        pkgs.haskell.lib.doJailbreak (self.callHackage "brick" "0.71.1" { });

      # latest master supports ghc 9.4.x
      tasty-test-reporter = packageFromSources "tasty-test-reporter";

      # latest master supports ghc 9.4.x
      string-qq = packageFromSources "string-qq";

      # jailbreak to allow text >= 2
      pretty-diff = pkgs.haskell.lib.doJailbreak super.pretty-diff;

      # servant-auth-server 0.4.8.0 is marked as broken in nixpkgs, so jailbreak
      # (known good) 0.4.7.0 to make it work
      servant-auth-server = pkgs.haskell.lib.doJailbreak
        (self.callHackage "servant-auth-server" "0.4.7.0" { });

      # required by servant-auth-server@0.4.7.0
      jose = pkgs.haskell.lib.dontCheck (self.callHackage "jose" "0.9" { });
      jose-jwt = self.callHackage "jose-jwt" "0.9.5" { };
    });
}
