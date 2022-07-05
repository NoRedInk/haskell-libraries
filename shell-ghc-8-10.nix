let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
  # pkgs-unstable = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8107.extend (self: super: {
    safe-coloured-text = super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
    # NB: purpose of this is to get terminal-size-0.3.3 for ghcid, because of
    # this issue: https://github.com/ndmitchell/ghcid/issues/359.
    # we can remove this override if we bump nixpkgs past 21.11.
    # terminal-size = pghc8107.kgs-unstable.haskell.packages.ghc8107.terminal-size;
    terminal-size = pkgs.haskell.lib.overrideCabal super.terminal-size {
      version = "0.3.3";
      sha256 = "sha256-jBdMj6cgC+LK/9bSX3if08c/S3sCmJ8zKkLXkB/KYMM";
    };
  });
}
