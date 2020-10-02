#!/usr/bin/env bash

set -euo pipefail

LC_ALL=C.UTF-8 ormolu -m inplace nri-*/**/*.hs

if git status --porcelain | grep . ; then
  echo "Not all files were formatted with ormolu."
  echo "Run ./format.sh and commit the result."
  git diff --no-color
  exit 1
fi
