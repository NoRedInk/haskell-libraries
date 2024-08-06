{ sources, pkgs }:
self: super:
  let
    packageFromSources = name: self.callCabal2nix name sources."${name}" { };
  in {
    # todo: resolve breaking changes in brick >= 0.72, also jailbreak
    # to allow ghc 9.4.x
    brick =
      pkgs.haskell.lib.doJailbreak (self.callHackage "brick" "0.71.1" { });

    # >= 5.39 has breaking changes for brick@0.71.1; pin to 5.38 (known-good)
    vty = self.callHackage "vty" "5.38" { };

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

    # servant-auth@0.4.1.0 is known-good but now complains re: jose version
    # (now needs >= 0.10 but we've already used it successfully with 0.9).  we
    # can bypass this issue with a jailbreak
    servant-auth = pkgs.haskell.lib.doJailbreak (self.callHackage "servant-auth" "0.4.1.0" { });

    # for now, pin hw-kafka-client to 4.0.3; nixpkgs@release-24.05 provides 5.3.0
    hw-kafka-client = self.callHackage "hw-kafka-client" "4.0.3" { };
  }