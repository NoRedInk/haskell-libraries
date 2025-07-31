{ sources, pkgs }:
self: super:
  let
    packageFromSources = name: self.callCabal2nix name sources."${name}" { };
  in {
    # todo: resolve breaking changes in brick >= 0.72, also jailbreak
    # to allow ghc 9.4.x
    brick =
      pkgs.haskell.lib.doJailbreak (self.callHackage "brick" "0.71.1" { });

    # >= 5.39 has breaking changes for brick@0.71.1; pin to 5.38 (known-good).
    # jailbreak to allow deepseq 1.5.1.0 as provided by ghc 9.8.x
    vty =
      pkgs.haskell.lib.doJailbreak (self.callHackage "vty" "5.38" { });

    # latest master supports ghc 9.4.x
    tasty-test-reporter = packageFromSources "tasty-test-reporter";

    # required by tasty-test-reporter
    ansi-terminal = self.callHackage "ansi-terminal" "1.0.2" { };
    ansi-terminal-types = self.callHackage "ansi-terminal-types" "0.11.5" { };
    tasty = self.callHackage "tasty" "1.4.3" { };
    tasty-quickcheck = self.callHackage "tasty-quickcheck" "0.10.2" { };

    # test suites have a hard requirement on tasty >= 1.5
    time-compat = pkgs.haskell.lib.dontCheck super.time-compat;
    wherefrom-compat = pkgs.haskell.lib.dontCheck super.wherefrom-compat;

    # latest master supports ghc 9.4.x
    string-qq = packageFromSources "string-qq";

    # jailbreak to allow text >= 2
    pretty-diff = pkgs.haskell.lib.doJailbreak super.pretty-diff;

    # for now, pin hw-kafka-client to 4.0.3; nixpkgs@release-25.05 provides 5.3.0
    hw-kafka-client = self.callHackage "hw-kafka-client" "4.0.3" { };
  }