#!/usr/bin/env bash

set -euo pipefail

function fail {
  echo "$1"
  exit 1
}

if [ $# -eq 0 ]; then
  fail "usage: release.sh <project>"
fi

pushd "$1"

name=$(grep name: < package.yaml | awk '{print $2}')
version=$(grep version: < package.yaml | awk '{print $2}')
bundle="$name-$version.tar.gz"

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md > /dev/null || fail "CHANGELOG.md is missing an entry for the current version."

# check copyright year is current year
grep "^copyright: $(date +'%Y')" < package.yaml > /dev/null \
  || fail "The copyright line in package.yaml does not match the current year."
grep "Copyright (c) $(date +'%Y')" < LICENSE > /dev/null \
  || fail "The copyright line in the LICENSE file does not match the current year."

# check github release tag exists
git fetch --tags
git tag -l --points-at HEAD | grep "^$name-$version$" > /dev/null \
  || fail "No git tag for current version exists. Please create tag with name: $name-$version"

hpack
cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
# Documentation upload doesn't seem to work, not sure why. If you find a fix
# please uncomment! Until then we rely on documentation generation from hackage.
# cabal upload -d --publish
