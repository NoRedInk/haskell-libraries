let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc926.extend (self: super: {
    # todo: resolve breaking changes in brick >= 0.72
    brick = self.callHackage "brick" "0.71.1" { };
  });
}
