#!/usr/bin/env bash

set -euxo pipefail

## start postgres
PGDATA="$(realpath _build/postgres/data)"
export PGDATA
export PGUSER=$USER
pg_ctl stop || true
rm -rf "$PGDATA"
mkdir -p "$PGDATA"
initdb
pg_ctl start -o '-k .'

## start redis
mkdir -p ./_build/redis/data
redis-server --daemonize yes --dir ./_build/redis/data

cabal build all
cabal test all
