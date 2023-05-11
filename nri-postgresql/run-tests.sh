#!/usr/bin/env bash

set -euxo pipefail

source setup-postgres.sh

cabal build
cabal test
