{ pkgs, haskellPackages }:

let
  sources = import ./sources.nix { };
  customHaskellPackages = haskellPackages.extend (self: super: {
    safe-coloured-text = super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
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
    pkgs.cabal-install
    pkgs.cachix
    pkgs.gnumake
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.niv
    pkgs.ormolu
    pkgs.pcre
    pkgs.redis
    pkgs.zlib
  ];
}
