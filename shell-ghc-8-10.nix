let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs {
    ovelays = [
      (self: super: {
        rdkafka = self.stdenv.mkDerivation rec {
          pname = "rdkafka";
          version = "2.2.0";

          # git clone https://github.com/confluentinc/librdkafka ../librdkafka
          src = ../librdkafka;

          nativeBuildInputs = with self.pkgs; [ pkg-config python3 which ];

          buildInputs = with self.pkgs; [ zlib zstd openssl ];

          env.NIX_CFLAGS_COMPILE = "-Wno-error=strict-overflow";

          postPatch = ''
            patchShebangs .
          '';

          enableParallelBuilding = true;
        };
      })
    ];
  };
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
