{ pkgs, haskellPackages }:

let sources = import ./sources.nix { };

in pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackges:
      with haskellPackges; [
        aeson
        aeson-pretty
        async
        auto-update
        base
        brick
        bugsnag-hs
        bytestring
        containers
        directory
        exceptions
        filepath
        fuzzy
        ghc
        hedgehog
        (pkgs.haskell.lib.dontCheck (callCabal2nix "hedis" sources.hedis { }))
        hostname
        http-client
        http-client-tls
        io-streams
        junit-xml
        microlens
        modern-uri
        network-uri
        pcre-light
        (callCabal2nix "pretty-diff" sources.pretty-diff { })
        pretty-show
        process
        random
        (callCabal2nix "safe-coloured-text"
          "${sources.safe-coloured-text}/safe-coloured-text" { })
        safe-exceptions
        stm
        terminal-size
        text
        text-zipper
        time
        unordered-containers
        vector
        vty
      ]))
    pkgs.cabal-install
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
