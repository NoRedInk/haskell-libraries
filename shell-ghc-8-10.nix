let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { };
in import nix/mk-shell.nix {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.ghc8104.extend (self: super: {
    bugsnag-hs = super.callHackageDirect {
      pkg = "bugsnag-hs";
      ver = "0.2.0.7";
      sha256 = "sha256-OVNuDmwXAVvQ3E5JMCM0y6b/4DsecbOT1tuz6htxUuo=";
    } { };
    scientific = super.callHackageDirect {
      pkg = "scientific";
      ver = "0.3.7.0";
      sha256 = "09iwj0snmx7vj7x03l4vdcn76zylcgxd9pyz0yxkydgfnn3lvc08";
    } { };
    semialign = super.callHackageDirect {
      pkg = "semialign";
      ver = "1.2";
      sha256 = "1crxnbz66k1qw8yrial8qa50z7q538q618ml1n4c5yghvpdgs1kx";
    } { };
    time-compat = super.callHackageDirect {
      pkg = "time-compat";
      ver = "1.9.6.1";
      sha256 = "0ika8xx9zff8rwaabs17q5c30c1b9ii89jhbvahi5nk7rs0cd5fs";
    } { };
    hashable = super.callHackageDirect {
      pkg = "hashable";
      ver = "1.3.4.1";
      sha256 = "13f2hy8jr519avnv9kg5hfx2n5s1l5d7786zfyj6w3ax77nsi8bm";
    } { };
    aeson-pretty =
      super.callCabal2nix "aeson-pretty" "${sources.aeson-pretty}" { };
    aeson = super.callHackageDirect {
      pkg = "aeson";
      ver = "2.0.1.0";
      sha256 = "0nhzbnygj17m4x39mmf8r13xisc0hnkijnrwyqskf8gk276x9dpz";
    } { };
    unordered-containers = super.callHackageDirect {
      pkg = "unordered-containers";
      ver = "0.2.14.0";
      sha256 = "1n4z4sh11jya0v55rhs2fmq787sj8qw4g1v6m0jj3z0p8jkm1mzw";
    } { };

    safe-coloured-text = super.callCabal2nix "safe-coloured-text"
      "${sources.safe-coloured-text}/safe-coloured-text" { };
    safe-coloured-text-terminfo =
      super.callCabal2nix "safe-coloured-text-terminfo"
      "${sources.safe-coloured-text}/safe-coloured-text-terminfo" { };
  });
}
