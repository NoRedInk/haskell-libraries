#!/usr/bin/env bash

set -euxo pipefail

## start postgres
mkdir -p "_build/postgres/data"
PGDATA="$(realpath _build/postgres/data)"
export PGDATA
export PGUSER=$USER
pg_ctl stop || true
rm -rf "$PGDATA"
initdb --no-locale --encoding=UTF8
pg_ctl start -o '-k .'

## start redis
mkdir -p ./_build/redis/data
redis-server --daemonize yes --dir ./_build/redis/data

## start kafka
server_properties_path=$(dirname "$(which kafka-server-start.sh)")/../config/server.properties
kafka-server-start.sh -daemon "$server_properties_path" --override num.partitions=10

cabal build all
cabal test all
