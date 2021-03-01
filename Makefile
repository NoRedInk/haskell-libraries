all: nri-prelude/nri-prelude.cabal nri-env-parser/nri-env-parser.cabal nri-log-explorer/nri-log-explorer.cabal

nri-prelude/nri-prelude.cabal: nri-prelude/package.yaml
	hpack nri-prelude

nri-env-parser/nri-env-parser.cabal: nri-env-parser/package.yaml
	hpack nri-env-parser

nri-log-explorer/nri-log-explorer.cabal: nri-log-explorer/package.yaml
	hpack nri-log-explorer
