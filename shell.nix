let
  fetch = { rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = sha256;
    };
  path = fetch ({
    # This comes from https://nixos.org/channels/
    #
    # Pick a release (e.g. nixpkgs-19.09-darwin) and open the `git-revision`
    # file. It will contain a revision hash. Copy and paste it below.
    #
    # Example: https://releases.nixos.org/nixpkgs/19.09-darwin/nixpkgs-darwin-19.09pre193902.c8ff042dac8/git-revision
    #
    # We usually go with the package name containing the -darwin suffix,
    # because it typically contains *both* macOS and Linux content, but
    # usually the one without the -darwin suffix only has Linux content.
    #
    rev = "244286efbc94fa6ce79a735a997df1f0370260b3";

    # Generate the SHA256 hash for this revision's tarball.
    #
    #   $ nix-prefetch-url --unpack --type sha256 \
    #   >   https://github.com/NixOS/nixpkgs/archive/${rev-defined-above}.tar.gz
    #
    # Example:
    #   $ nix-prefetch-url --unpack --type sha256 \
    #   >   https://github.com/NixOS/nixpkgs/archive/c8ff042dac86a4dee0c3d4bbed5838156f041548.tar.gz
    #
    sha256 = "1jy80bdvdxv604kf9fchibyzzqslm6vxddz9dnyrm73m6cz57cn8";
  });

  pkgs = import path { };

in pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskellPackages.hpack
    pkgs.zlib
    pkgs.haskellPackages.ghcid
    pkgs.ormolu
  ];
}
