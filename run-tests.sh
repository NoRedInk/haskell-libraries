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

## start zookeeper (for kafka)
zk_server_properties_path=$(dirname "$(which zkServer.sh)")/../conf/zoo_sample.cfg
mkdir -p /tmp/zookeeper /tmp/zookeeper-logs
ZOOPIDFILE=/tmp/zookeeper-logs/pid \
          ZOO_LOG_DIR=/tmp/zookeeper-logs \
          zkServer.sh stop "$zk_server_properties_path"
rm -rf /tmp/zookeeper/* /tmp/zookeeper-logs/*
ZOOPIDFILE=/tmp/zookeeper-logs/pid \
          ZOO_LOG_DIR=/tmp/zookeeper-logs \
          zkServer.sh start "$zk_server_properties_path"

## wait for zookeeper
echo "waiting for zookeeper to start"
until nc -vz localhost 2181
do
  sleep 1
done
echo "zookeeper available"

## start kafka
kafka-server-stop.sh || true # but first stop it
rm -rf /tmp/kafka-logs
server_properties_path=$(dirname "$(which kafka-server-start.sh)")/../config/server.properties
kafka-server-start.sh -daemon "$server_properties_path" --override num.partitions=10

## wait for kafka
echo "waiting for kafka to start"
until  nc -vz localhost 9092
do
  sleep 1
done
echo "kafka available"


cabal build all
cabal test all

# cleanup
kafka-server-stop.sh
ZOOPIDFILE=/tmp/zookeeper-logs/pid \
          ZOO_LOG_DIR=/tmp/zookeeper-logs  \
          zkServer.sh stop "$zk_server_properties_path"
