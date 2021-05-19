#!/usr/bin/env bash

set -euo pipefail
shopt -s globstar

# Check all Haskell files are formatted using Ormolu
LC_ALL=C.UTF-8 ormolu -m inplace nri-*/**/*.hs
if git status --porcelain | grep . ; then
  echo "Not all files were formatted with ormolu."
  echo "To fix this error, run ./test.sh and commit the result."
  git diff --no-color
  exit 1
fi

# Run Haskell tests
make cabal
if git status --porcelain | grep . ; then
  echo "changed package.yaml without checking in then generated .cabal file."
  echo "To fix this error, run \`make cabal\` and commit the result."
  git diff --no-color
  exit 1
fi

./run-tests.sh
