let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8107.extend (self: super: {
    # modern-uri-0.3.6.0 is incompatible with the `base` provided by ghc 8.10.7;
    # use known-good @ 0.3.4.4.
    modern-uri = self.callHackage "modern-uri" "0.3.4.4" { };

    # todo: resolve breaking changes in brick >= 0.72
    brick = self.callHackage "brick" "0.71.1" { };
  });
}
