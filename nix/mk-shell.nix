{ pkgs, haskellPackages }:

# Fix from https://github.com/srid/haskell-template
let
  sources = import ./sources.nix { };
  pkgs-unstable = import sources.nixpkgs-unstable { };
  workaround140774 = hpkg:
    with pkgs.haskell.lib;
    overrideCabal hpkg (drv: { enableSeparateBinOutput = false; });
  haskell-language-server =
    pkgs.haskellPackages.haskell-language-server.override {
      hls-ormolu-plugin = pkgs.haskellPackages.hls-ormolu-plugin.override {
        ormolu = (workaround140774 pkgs.haskellPackages.ormolu);
      };
    };

in pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackages:
      with haskellPackages; [
        aeson
        aeson-pretty
        async
        auto-update
        base
        brick
        bugsnag-hs
        bytestring
        conduit
        containers
        directory
        exceptions
        filepath
        fuzzy
        ghc
        hedgehog
        hedis
        hostname
        http-client
        http-client-tls
        hw-kafka-client
        io-streams
        junit-xml
        microlens
        modern-uri
        network-uri
        pcre-light
        postgresql-typed
        pretty-diff
        pretty-show
        process
        random
        resourcet
        safe-coloured-text
        safe-coloured-text-terminfo
        safe-exceptions
        servant
        servant-auth-server
        servant-server
        stm
        terminal-size
        text
        text-zipper
        time
        th-test-utils
        unordered-containers
        uuid
        vector
        vty
      ]))
    (workaround140774 pkgs.haskellPackages.ghcid)
    (workaround140774 pkgs.haskellPackages.niv)
    (workaround140774 pkgs.haskellPackages.ormolu)
    pkgs.apacheKafka # for nri-kafka
    pkgs.cabal-install
    pkgs.cachix
    pkgs.gnumake
    haskell-language-server
    pkgs.haskellPackages.hpack
    pkgs.pcre
    pkgs.postgresql # for nri-postgres
    pkgs.redis # for nri-redis
    pkgs.zlib
    pkgs-unstable.zookeeper # for nri-kafka
  ];
}
