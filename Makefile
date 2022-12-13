all: cabal

cabal: nri-prelude/nri-prelude.cabal nri-env-parser/nri-env-parser.cabal nri-log-explorer/nri-log-explorer.cabal nri-observability/nri-observability.cabal nri-redis/nri-redis.cabal nri-test-encoding/nri-test-encoding.cabal nri-http/nri-http.cabal nri-postgresql/nri-postgresql.cabal nri-kafka/nri-kafka.cabal

nri-env-parser/nri-env-parser.cabal: nri-env-parser/package.yaml
	hpack nri-env-parser

ghcid-nri-env-parser: nri-env-parser/nri-env-parser.cabal
	cd nri-env-parser && ghcid

ghcid-nri-env-parser-test: nri-env-parser/nri-env-parser.cabal
	cd nri-env-parser && ghcid --command "cabal repl nri-env-parser:test:tests" --test Main.main

nri-http/nri-http.cabal: nri-http/package.yaml
	hpack nri-http

ghcid-nri-http: nri-http/nri-http.cabal
	cd nri-http && ghcid

nri-kafka/nri-kafka.cabal: nri-kafka/package.yaml
	hpack nri-kafka

ghcid-nri-kafka: nri-kafka/nri-kafka.cabal
	cd nri-kafka && ghcid

ghcid-nri-kafka-test: nri-kafka/nri-kafka.cabal
	cd nri-kafka && ghcid --command "cabal repl nri-kafka:test:tests" --test Main.main

nri-log-explorer/nri-log-explorer.cabal: nri-log-explorer/package.yaml
	hpack nri-log-explorer

ghcid-nri-log-explorer: nri-log-explorer/nri-log-explorer.cabal
	cd nri-log-explorer && ghcid

nri-observability/nri-observability.cabal: nri-observability/package.yaml
	hpack nri-observability

ghcid-nri-observability: nri-observability/nri-observability.cabal
	cd nri-observability && ghcid

ghcid-nri-observability-test: nri-observability/nri-observability.cabal
	cd nri-observability && ghcid --command "cabal repl nri-observability:test:tests" --test Main.main

nri-prelude/nri-prelude.cabal: nri-prelude/package.yaml
	hpack nri-prelude

ghcid-nri-prelude-test: nri-prelude/nri-prelude.cabal
	cd nri-prelude && ghcid --command "cabal repl nri-prelude:test:tests" --test Main.main

ghcid-nri-prelude: nri-prelude/nri-prelude.cabal
	cd nri-prelude && ghcid

nri-postgresql/nri-postgresql.cabal: nri-postgresql/package.yaml
	hpack nri-postgresql

ghcid-nri-postgresql: nri-postgresql/nri-postgresql.cabal
	cd nri-postgresql && ghcid

nri-redis/nri-redis.cabal: nri-redis/package.yaml
	hpack nri-redis

ghcid-nri-redis: nri-redis/nri-redis.cabal
	cd nri-redis && ghcid

ghcid-nri-redis-test: nri-redis/nri-redis.cabal
	cd nri-redis && ghcid --command "cabal repl nri-redis:test:tests" --test Main.main

nri-test-encoding/nri-test-encoding.cabal: nri-test-encoding/package.yaml
	hpack nri-test-encoding

ghcid-nri-test-encoding: nri-test-encoding/nri-test-encoding.cabal
	cd nri-test-encoding && ghcid
