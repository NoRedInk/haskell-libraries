#!/usr/bin/env bash

set -euxo pipefail

pushd nri-postgresql
source setup-postgres.sh
popd

## start redis
mkdir -p ./_build/redis/data
redis-server --daemonize yes --dir ./_build/redis/data

## start zookeeper (for kafka) 
mkdir -p /tmp/zookeeper /tmp/zookeeper-logs
ZOOPIDFILE=/tmp/zookeeper-logs/pid ZOO_LOG_DIR=/tmp/zookeeper-logs  zkServer.sh stop zoo_sample.cfg
rm -rf /tmp/zookeeper/* /tmp/zookeeper-logs/*
ZOOPIDFILE=/tmp/zookeeper-logs/pid ZOO_LOG_DIR=/tmp/zookeeper-logs zkServer.sh start zoo_sample.cfg

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
ZOOPIDFILE=/tmp/zookeeper-logs/pid ZOO_LOG_DIR=/tmp/zookeeper-logs  zkServer.sh stop zoo_sample.cfg
