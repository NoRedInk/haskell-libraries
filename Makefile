all: cabal

cabal: nri-prelude/nri-prelude.cabal nri-env-parser/nri-env-parser.cabal nri-log-explorer/nri-log-explorer.cabal nri-observability/nri-observability.cabal nri-redis/nri-redis.cabal nri-test-encoding/nri-test-encoding.cabal

nri-observability/nri-observability.cabal: nri-observability/package.yaml
	hpack nri-observability

nri-prelude/nri-prelude.cabal: nri-prelude/package.yaml
	hpack nri-prelude

nri-env-parser/nri-env-parser.cabal: nri-env-parser/package.yaml
	hpack nri-env-parser

nri-log-explorer/nri-log-explorer.cabal: nri-log-explorer/package.yaml
	hpack nri-log-explorer

nri-redis/nri-redis.cabal: nri-redis/package.yaml
	hpack nri-redis

ghcid-nri-prelude-test: nri-prelude-test/nri-prelude-test.cabal

nri-test-encoding/nri-test-encoding.cabal: nri-test-encoding/package.yaml
	hpack nri-test-encoding

ghcid-nri-prelude-test:
	cd nri-prelude && ghcid --command "cabal repl nri-prelude:test:tests" --test Main.main

ghcid-nri-prelude: nri-prelude/nri-prelude.cabal
	cd nri-prelude && ghcid

ghcid-nri-log-explorer: nri-log-explorer/nri-log-explorer.cabal
	cd nri-log-explorer && ghcid

ghcid-nri-env-parser: nri-env-parser/nri-env-parser.cabal
	cd nri-env-parser && ghcid

ghcid-nri-observability: nri-observability/nri-observability
	cd nri-observability && ghcid

ghcid-nri-redis: nri-redis/nri-redis.cabal
	cd nri-redis && ghcid

ghcid-nri-redis-test: nri-redis/nri-redis.cabal
	cd nri-redis && ghcid --command "cabal repl nri-redis:test:tests" --test Main.main

ghcid-nri-test-encoding:
	cd nri-test-encoding && ghcid
