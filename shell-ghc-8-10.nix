let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8107.extend (self: super: {
    aeson = super.aeson_2_0_1_0;
    semialign = super.semialign_1_2_0_1;
    time-compat = super.time-compat_1_9_6_1;
    postgresql-binary = super.postgresql-binary_0_12_4_2;
    hashable-time = super.hashable-time_0_3;
    hashable = super.callHackageDirect {
      pkg = "hashable";
      ver = "1.3.4.1";
      sha256 = "13f2hy8jr519avnv9kg5hfx2n5s1l5d7786zfyj6w3ax77nsi8bm";
    } { };
    safe-coloured-text = super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
  });
}
