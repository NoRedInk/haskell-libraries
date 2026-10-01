{ sources, pkgs }:
self: super:
  let
    packageFromSources = name: self.callCabal2nix name sources."${name}" { };

    hlsPkgSrc =
      {
        subPkg ? null,
        version ? "2.14.0.0",
      }:
      if super.haskell-language-server.version != "2.13.0.0" then
        throw "expected haskell-language-server from nixpkgs to be 2.13.0.0, got ${super.haskell-language-server.version} instead.  please remove the niv source for haskell-language-server and use the one directly from nixpkgs instead"
      else
        {
          inherit version;
          src =
            if subPkg == null then
              sources.haskell-language-server-2_14_0_0
            else
              "${sources.haskell-language-server-2_14_0_0}/${subPkg}";
        };
  in {
    # todo: resolve breaking changes in brick >= 0.72, also jailbreak
    # to allow ghc 9.4.x
    brick =
      pkgs.haskell.lib.doJailbreak (self.callHackage "brick" "0.71.1" { });

    # >= 5.39 has breaking changes for brick@0.71.1; pin to 5.38 (known-good).
    # jailbreak to allow deepseq 1.5.1.0 as provided by ghc 9.8.x
    vty =
      pkgs.haskell.lib.doJailbreak (self.callHackage "vty" "5.38" { });

    # latest master supports ghc 9.4.x, but has packages bounds not compatible
    # with ghc 9.10.x
    tasty-test-reporter =
      pkgs.haskell.lib.doJailbreak (packageFromSources "tasty-test-reporter");

    # required by tasty-test-reporter
    ansi-terminal = self.callHackage "ansi-terminal" "1.0.2" { };
    ansi-terminal-types = self.callHackage "ansi-terminal-types" "0.11.5" { };
    tasty = self.callHackage "tasty" "1.4.3" { };
    tasty-quickcheck = self.callHackage "tasty-quickcheck" "0.10.2" { };
    tasty-rerun = self.callHackage "tasty-rerun" "1.1.19" { };

    # test suites have a hard requirement on tasty >= 1.5
    binary-instances = pkgs.haskell.lib.dontCheck super.binary-instances;
    hashable = pkgs.haskell.lib.dontCheck super.hashable;
    lukko = pkgs.haskell.lib.dontCheck super.lukko;
    time-compat = pkgs.haskell.lib.dontCheck super.time-compat;
    wherefrom-compat = pkgs.haskell.lib.dontCheck super.wherefrom-compat;

    # jailbreak to allow text >= 2
    pretty-diff = pkgs.haskell.lib.doJailbreak super.pretty-diff;

    # for now, pin hw-kafka-client to 4.0.3; nixpkgs@release-25.05 provides 5.3.0
    hw-kafka-client = self.callHackage "hw-kafka-client" "4.0.3" { };

    # marked broken in nixpkgs
    # (nixpkgs@release-25.11 provides io-classes-1.8.0.1 which renamed
    # InspectMonad -> InspectMonadSTM); patch fixes the renamed type
    strict-stm = pkgs.haskell.lib.doJailbreak (
      pkgs.haskell.lib.appendPatch (self.callHackage "strict-stm" "1.5.0.0"
        { }
      ) ./patches/strict-stm-1_5_0_0-inspect-monad-fix.patch
    );

    # nixpkgs provides `tls@2.1.8` but this is actually deprecated according to
    # hackage anyway (lol).  we don't over-upgrade to `2.3.x` or `2.4.x` because
    # those have `crypton-* >=1.9.0` and `ram` as constraints (relevant for our monorepo)
    tls = self.callHackage "tls" "2.2.2" { };

    # required by the above
    crypton-x509 = self.callHackage "crypton-x509" "1.8.0" { };
    crypton-x509-validation = self.callHackage "crypton-x509-validation" "1.8.0" { };
    crypton-x509-store = self.callHackage "crypton-x509-store" "1.8.0" { };

    # support for hls + ghc 9.12.4; we shouldn't need to do all of this
    # in nixpkgs >= 26.11 as long as hls >= 2.14.0.0 is provided.
    # optimizations are disabled for some packages as a workaround to
    # avoid e.g. `lookupIdSubst` panic (ghcide), `Iface id out of scope:  ww`
    # (hls-test-utils)
    hie-bios = pkgs.haskell.lib.dontCheck self.hie-bios_0_19_0;
    hiedb = self.hiedb_0_8_0_0;
    lsp = self.lsp_2_8_0_0;
    lsp-test = pkgs.haskell.lib.dontCheck self.lsp-test_0_18_0_0;
    unordered-containers = self.unordered-containers_0_2_21;
    ghcide = pkgs.haskell.lib.disableOptimization (
      pkgs.haskell.lib.overrideSrc super.ghcide (hlsPkgSrc {
        subPkg = "ghcide";
      })
    );
    hls-graph = pkgs.haskell.lib.overrideSrc super.hls-graph (hlsPkgSrc {
      subPkg = "hls-graph";
    });
    hls-plugin-api = pkgs.haskell.lib.addBuildTools (pkgs.haskell.lib.overrideSrc
      super.hls-plugin-api
      (hlsPkgSrc {
        subPkg = "hls-plugin-api";
      })
    ) [ pkgs.git ]; # for tests
    hls-test-utils = pkgs.haskell.lib.disableOptimization (
      pkgs.haskell.lib.overrideSrc super.hls-test-utils (hlsPkgSrc {
        subPkg = "hls-test-utils";
      })
    );
    haskell-language-server = pkgs.haskell.lib.overrideSrc super.haskell-language-server (
      hlsPkgSrc { }
    );
  }