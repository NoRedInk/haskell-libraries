#!/usr/bin/env bash

set -euo pipefail

if [ $# -eq 0 ]; then
  echo "usage: release.sh <project>"
  exit 1
fi

pushd "$1"

name=$(cat package.yaml | grep name: | awk '{print $2}')
version=$(cat package.yaml | grep version: | awk '{print $2}')
bundle="$name-$version.tar.gz"

hpack
cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
# Documentation upload doesn't seem to work, not sure why. If you find a fix
# please uncomment! Until then we rely on documentation generation from hackage.
# cabal upload -d --publish
