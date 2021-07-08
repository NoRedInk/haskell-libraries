{ pkgs, haskellPackages, useServant_0_18_3 ? false
, customSafeColouredText ? true }:

let
  sources = import ./sources.nix { };
  customHaskellPackages = haskellPackages.extend (self: super: {
    safe-coloured-text = if customSafeColouredText then
      super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { }
    else
      self.safe-coloured-text;
    safe-coloured-text-terminfo = if customSafeColouredText then
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { }
    else
      self.safe-coloured-text-terminfo;

    servant = if useServant_0_18_3 then self.servant_0_18_3 else self.servant;
    servant-server = if useServant_0_18_3 then
      self.servant-server_0_18_3
    else
      self.servant-server;
  });
in pkgs.mkShell {
  buildInputs = [
    (customHaskellPackages.ghcWithPackages (haskellPackges:
      with haskellPackges; [
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
