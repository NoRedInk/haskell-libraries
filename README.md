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
ormolu -i <file>                  # Format a source file
cabal haddock nri-prelude         # Run documentation generation
ormolu -i **/*.hs                 # Format everything
cabal build all                   # build everything
cabal test all                    # test everything
```

We use [Ormolu][ormolu] for code formatting.

Documentation comments use the [Haddock][haddock] format to ensure they are rendered correctly on package websites.

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

### To [hackage.org][hackage]

[Hackage][hackage] (the Haskell package repository) uses a versioning scheme called [PVP][pvp]. It's like the 'semantic versioning' used by NPM, Elm, and others, but differs in having two major version digits rather than one:

    MAJOR.MAJOR.MINOR.PATCH

To keep things simple for those familiar with semantic versioning this repository uses the convention of keeping the first major number zero. When creating a new version change the other version digits as you would using semantic versioning.

To publish the new version run the `release.sh` script for the package you want to publish:

```sh
./release.sh nri-prelude
```

Note: this requires an account on [hackage.org][hackage] with rights to publish the library. These are the steps to creating such an account:

1. [Fill the registration form][hackage-registration].
2. Send an email to hackage-trustees@haskell.org to requesting upload access for your account.
3. Login to hackage using the shared NoRedInk account (NRI engineers can find the password in the usual place).
4. Using the NoRedInk account add your new own hackage account to the [maintainer groups][hackage-maintainers] of the NRI libraries.

### To [stackage.org][stackage]

[Stackage][stackage] is a repository built on top of hackage. Stackage runs nightly builds checking the latest version of packages are compiling against each other.

We registered the libraries in this repository with Stackage [here][nri-on-stackage]. Stackage will find new versions of these libraries pushed to hackage.

To remain in stackage these libraries need to up-to-date with the latest versions of their dependencies. When a dependency releases a new version not accepted by the version bounds of a package in this library then we need to do a new release to support the new version of the dependency. To get notifications when we need to do an update like that, join the Haskell Open Source Maintainers NRI mailing group.

[nix]: https://nixos.org/
[direnv]: https://direnv.net/
[hackage]: https://hackage.haskell.org/
[pvp]: https://pvp.haskell.org/faq/
[stackage]: https://www.stackage.org/
[ormolu]: https://github.com/tweag/ormolu
[haddock]: https://haskell-haddock.readthedocs.io
[nri-on-stackage]: https://github.com/commercialhaskell/stackage/blob/b9c0bfa723bd4cba5f964c6fb99b7528c4027692/build-constraints.yaml#L4414-L4416
[hackage-registration]: https://hackage.haskell.org/users/register-request
[hackage-maintainers]: https://hackage.haskell.org/user/NoRedInk
