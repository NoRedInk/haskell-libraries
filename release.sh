#!/usr/bin/env bash

set -euo pipefail

function fail {
  echo "$1"
  exit 1
}

if [ $# -eq 0 ]; then
  fail "usage: release.sh <project>"
fi

if [ "$1" = "nri-log-explorer" ]; then
  fail "no need to publish nri-log-explorer, because it's a tool rather than a library."
fi

if git status --porcelain | grep . ; then
  fail "Stash any changes before starting the release script."
fi

pushd "$1"

name=$(grep name: < package.yaml | awk '{print $2}')
version=$(grep version: < package.yaml | awk '{print $2}')
bundle="$name-$version.tar.gz"

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md > /dev/null || fail "CHANGELOG.md is missing an entry for the current version. The line must look exactly like: # $version"

# check copyright year is current year
grep "^copyright: $(date +'%Y')" < package.yaml > /dev/null \
  || fail "The copyright line in package.yaml does not match the current year."
grep "Copyright (c) $(date +'%Y')" < LICENSE > /dev/null \
  || fail "The copyright line in the LICENSE file does not match the current year."

hpack
if git status --porcelain | grep . ; then
  fail "Cabal file changed after running hpack. Check in latest cabal file before releasing."
fi

# check github release tag exists
git fetch --tags
git tag -l --points-at HEAD | grep "^$name-$version$" > /dev/null \
  || fail "No git tag for current version exists. Please create tag:

$ git tag $name-$version && git push --tags"

cabal sdist -o - > "$bundle"
cabal haddock --haddock-for-hackage --haddock-hyperlink-source --haddock-quickjump
cabal upload --publish "$bundle"
cabal upload -d --publish "../dist-newstyle/$name-$version-docs.tar.gz"
