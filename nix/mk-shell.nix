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
        ghc
        hedgehog
        hostname
        http-client
        http-client-tls
        junit-xml
        modern-uri
        network-uri
        pretty-show
        process
        safe-exceptions
        terminal-size
        text
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
  ];
}
