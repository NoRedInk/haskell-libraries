#!/usr/bin/env bash

set -euxo pipefail

. setup-postgres.sh

cabal build
cabal test
