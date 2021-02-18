let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (haskellPackges:
      with haskellPackges; [
        (callCabal2nix "pretty-diff" sources.pretty-diff { })
        aeson
        aeson-pretty
        ansi-terminal
        async
        auto-update
        base
        brick
        bytestring
        containers
        directory
        exceptions
        filepath
        ghc
        hedgehog
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
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.ormolu
  ];
}
