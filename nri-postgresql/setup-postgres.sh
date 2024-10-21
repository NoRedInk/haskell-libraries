#!/usr/bin/env bash

## start postgres
mkdir -p "../_build/postgres/data"
PGDATA="$(realpath ../_build/postgres/data)"
export PGDATA
export PGUSER=$USER
source cleanup-postgres.sh
initdb --no-locale --encoding=UTF8
pg_ctl start -o '-k . -p 5432'

createdb testdb --host=localhost --port=5432 --username=$USER

export PGHOST=localhost
export PGPORT=5432
export PGDATABASE=testdb

## Setup for test/Enum.hs
psql -c "CREATE TYPE test_enum as ENUM ('value_1', 'value_2')" || true
psql -c "CREATE TABLE test_table (enum_col test_enum NOT NULL)" || true
psql -c "CREATE TABLE test_table2 (enum_array_col test_enum[] NOT NULL)" || true

## Setup for test/Test.hs
psql -c "CREATE TABLE constraints_table (user_id int PRIMARY KEY)" || true

## Setup for test/ObservabilitySpec.hs
createuser -s postgres
psql -c "GRANT ALL PRIVILEGES ON DATABASE testdb TO postgres;" || true
