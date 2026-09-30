let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  commonHaskellOverrides = import ./nix/common-haskell-overrides.nix { inherit sources pkgs; };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc9124.extend (self: super:
    commonHaskellOverrides self super // {
      # HDBC @ 2.4.0.4 requires time <1.14, but ghc 9.12.4 uses time ==1.14.  this
      # is fixed with HDBC @ 2.4.0.5
      HDBC =
        if super.HDBC.version != "2.4.0.4" then
          throw "expected HDBC from nixpkgs to be 2.4.0.4, got ${super.HDBC.version} instead.  this could be good!  if it's newer, please remove the niv source for HDBC and use the one directly from nixpkgs"
        else
          self.callCabal2nix "HDBC" sources.HDBC-2_4_0_5 { };

      # almost all tests pass
      xml-conduit = pkgs.haskell.lib.dontCheck super.xml-conduit;
    }
  );
}
