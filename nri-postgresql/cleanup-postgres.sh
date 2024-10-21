#!/usr/bin/env bash

PGDATA="$(realpath ../_build/postgres/data)"
export PGDATA
export PGUSER=$USER
pg_ctl stop || true
rm -rf "$PGDATA"
