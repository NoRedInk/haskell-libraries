{ pkgs, haskellPackages }:

let sources = import ./sources.nix { };

in pkgs.mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (haskellPackges:
      with haskellPackges; [
        (callCabal2nix "pretty-diff" sources.pretty-diff { })
        aeson
        aeson-pretty
        ansi-terminal
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
        hostname
        http-client
        http-client-tls
        junit-xml
        microlens
        modern-uri
        network-uri
        pretty-show
        process
        random
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
    pkgs.haskellPackages.hpack
    pkgs.niv
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.ormolu
    pkgs.gnumake
  ];
}
