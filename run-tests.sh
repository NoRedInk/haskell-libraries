cabal build all

## start redis
mkdir -p ./_build/redis/data
redis-server --daemonize yes --dir ./_build/redis/data


cabal test all
