{ pkgs, haskellPackages }:

pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackges:
      with haskellPackges; [
        aeson_2_0_1_0
        aeson_2_0_1_0
        semialign_1_2_0_1
        time-compat_1_9_6_1
        postgresql-binary_0_12_4_2
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
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.niv
    pkgs.ormolu
    pkgs.pcre
    pkgs.redis # for nri-redis
    pkgs.postgresql # for nri-postgres
    pkgs.zlib
    pkgs.zookeeper # for nri-kafka
  ];
}
