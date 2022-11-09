#!/usr/bin/env bash

set -euxo pipefail

## start postgres
mkdir -p "../_build/postgres/data"
PGDATA="$(realpath ../_build/postgres/data)"
export PGDATA
export PGUSER=$USER
pg_ctl stop || true
rm -rf "$PGDATA"
initdb --no-locale --encoding=UTF8
pg_ctl start -o '-k .'


cabal build
cabal test