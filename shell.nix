let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
  pkgs-unstable = import sources.nixpkgs-unstable { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  pkgs-unstable = pkgs-unstable;
  haskellPackages = pkgs-unstable.haskell.packages.ghc924.extend (self: super: {
    safe-coloured-text = super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
  });
}
