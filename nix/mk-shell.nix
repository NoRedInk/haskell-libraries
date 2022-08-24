{ pkgs, pkgs-unstable, haskellPackages }:

# Fix from https://github.com/srid/haskell-template
let
  workaround140774 = hpkg:
    with pkgs.haskell.lib;
    overrideCabal hpkg (drv: { enableSeparateBinOutput = false; });

in pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackages:
      with haskellPackages; [
        # These tests take waaaaay too much time
        # We may want to revert this later
        (pkgs.haskell.lib.dontCheck servant)
        (pkgs.haskell.lib.dontCheck servant-auth-server)
        (pkgs.haskell.lib.dontCheck servant-server)
        (pkgs.haskell.lib.dontCheck text-zipper)
        (pkgs.haskell.lib.dontCheck vector)
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
        stm
        terminal-size
        text
        time
        unordered-containers
        uuid
        vty
      ]))
    (workaround140774 pkgs.haskellPackages.niv)
    pkgs-unstable.cabal-install
    pkgs-unstable.haskellPackages.ghcid
    pkgs-unstable.haskellPackages.haskell-language-server
    pkgs-unstable.haskellPackages.hpack
    pkgs-unstable.haskellPackages.ormolu
    pkgs.apacheKafka # for nri-kafka
    pkgs.cachix
    pkgs.gnumake
    pkgs.pcre
    pkgs.postgresql # for nri-postgres
    pkgs.redis # for nri-redis
    pkgs.zlib
    pkgs.zookeeper # for nri-kafka
  ];
}
