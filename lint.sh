#!/usr/bin/env bash

set -euo pipefail

# Check all Haskell files have been formatted using ormolu.
for file in nri-*/**/*.hs
do
  if ormolu -m check "$file"; then
    echo "$file is formatted using ormolu"
  else
    echo "$file is not formatted using ormolu."
    echo "Run 'ormolu -m inplace $file' to format it."
    echo "Then rerun './lint.sh' to see if there's additional formatting errors."
    exit 1
  fi
done
