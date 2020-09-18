# NoRedInk/haskell-libraries

A monorepo of our opensourced Haskell libraries.

## Developing

To set up the development environment for this project, install [Nix][nix] and [Direnv][direnv], then run:

```sh
direnv allow
```

Some useful shell commands when working on these libraries:

```sh
cabal build nri-prelude           # build the library
cabal test nri-prelude            # run the tests
hpack nri-prelude                 # generate nri-prelude.cabal from package.yaml
ghcid -c "cabal repl nri-prelude" # start a code watcher
```

### Tips for adding dependencies

To allow these libraries to be in [Stackage][stackage] we're limited to dependencies that are themselves in Stackage. Being in stackage indicates a package has an active maintainer, who keeps the package compatible with the latest versions of its dependencies. This means that while using stackage dependencies only limits us it also saves us time.

When we add a dependency we need to give it a lower and upper version bound. Here's some tips for finding reasonable initial values.

For the lower bound we can use a known-compatible version. If we're adding a dependency on `tropical-fruits` to `nri-prelude`, we can go into the project making use of `nri-prelude` and getting the version of `tropical-fruits` already in use there, by running:

```sh
$ ghc-pkg list | grep tropical-fruits
tropical-fruits-2.6.2.0
```

The output above would translate into a lower bound of `>= 2.6.2.0`.

For the upper bound we can go one major version above the last released version of the dependency. Were `tropical-fruits` a real package we could find it's latest version at: https://hackage.haskell.org/package/tropical-fruits

Once we have the latest version we can make an upper bound of that by keeping the first two digits and incrementing the second of those by one. For example: if the latest release is `2.7.1.1` our upper bound will be `< 2.8`. This works because the first two digits together make up the major version in the [PVP][pvp] versioning scheme used on hackage.

## Publishing

[Hackage][hackage] (the Haskell package repository) uses a versioning scheme called [PVP][pvp]. It's like the 'semantic versioning' used by NPM, Elm, and others, but differs in having two major version digits rather than one:

    MAJOR.MAJOR.MINOR.PATCH

To keep things simple for those familiar with semantic versioning this repository uses the convention of keeping the first major number zero. When creating a new version change the other version digits as you would using semantic versioning.

[nix]: https://nixos.org/
[direnv]: https://direnv.net/
[hackage]: https://hackage.haskell.org/
[pvp]: https://pvp.haskell.org/faq/
[stackage]: https://www.stackage.org/
