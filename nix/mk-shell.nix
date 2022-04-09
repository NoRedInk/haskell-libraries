{ pkgs, haskellPackages }:

# Fix from https://github.com/srid/haskell-template
let
  workaround140774 = hpkg:
    with pkgs.haskell.lib;
    overrideCabal hpkg (drv: { enableSeparateBinOutput = false; });

in pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackages:
      with haskellPackages; [
        aeson
        semialign_1_2_0_1
        time-compat_1_9_6_1
        postgresql-binary
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
        unordered-containers
        uuid
        vector
        vty
      ]))
    pkgs.apacheKafka # for nri-kafka
    pkgs.cabal-install
    pkgs.cachix
    pkgs.gnumake
    (workaround140774 pkgs.haskellPackages.ghcid)
    pkgs.haskellPackages.hpack
    (workaround140774 pkgs.haskellPackages.niv)
    (workaround140774 pkgs.haskellPackages.ormolu)
    pkgs.pcre
    pkgs.redis # for nri-redis
    pkgs.postgresql # for nri-postgres
    pkgs.zlib
    pkgs.zookeeper # for nri-kafka
  ];
}
