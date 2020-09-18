# NoRedInk/haskell-libraries

A monorepo of our opensourced Haskell libraries.

## Developing

To set up the development environment for this project, install [Nix][nix] and [Direnv][direnv], then run:

```
direnv allow
```

Some useful shell commands when working on these libraries:

```sh
cabal build nri-prelude           # build the library
cabal test nri-prelude            # run the tests
hpack nri-prelude                 # generate nri-prelude.cabal from package.yaml
ghcid -c "cabal repl nri-prelude" # start a code watcher
```

[nix]: https://nixos.org/
[direnv]: https://direnv.net/
