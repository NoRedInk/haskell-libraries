{ pkgs, haskellPackages }:

# Fix from https://github.com/srid/haskell-template
let
  workaround140774 = hpkg:
    with pkgs.haskell.lib;
    overrideCabal hpkg (drv: { enableSeparateBinOutput = false; });
  # It is still necessary to run `hpack --force` into packages home dirs
  haskell-language-server = pkgs.haskellPackages.haskell-language-server.override {
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
        auto-update
        base64-bytestring_1_2_1_0
        brick
        bugsnag-hs
        conduit
        fuzzy
        hedgehog
        hedis
        hostname
        http-client-tls
        http-types
        hw-kafka-client
        io-streams
        junit-xml
        lens
        mime-types
        modern-uri
        network-uri
        pcre-light
        postgresql-typed
        pretty-diff
        pretty-show
        resource-pool
        terminal-size
        safe-coloured-text-terminfo
        safe-exceptions
        servant-auth-server
        servant-server
        text-zipper
        uuid
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
    pkgs.zookeeper # for nri-kafka
  ];
}
