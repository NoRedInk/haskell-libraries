let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  commonHaskellOverrides = import ./nix/common-haskell-overrides.nix { inherit sources pkgs; };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc9122.extend (self: super:
    commonHaskellOverrides self super // {
      # package version bounds are nonsense without these flags set;
      # see https://github.com/hdbc/hdbc/blob/hdbc-2.4.0.2/HDBC.cabal
      #
      # note: still need to jailbreak via patch for `time >= 1.14`;
      # see https://github.com/NixOS/jailbreak-cabal/issues/7
      HDBC = with pkgs.haskell.lib;
        appendPatch (disableCabalFlag (enableCabalFlag (doJailbreak super.HDBC) "splitBase") "buildtests") ./nix/patches/hdbc-time-1.14.patch;

      # almost all tests pass
      xml-conduit = pkgs.haskell.lib.dontCheck super.xml-conduit;
    }
  );
}
