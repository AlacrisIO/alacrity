.PHONY: all build test lint start_geth stop_geth logs

export STACK_YAML=../../hs/alacrity/stack.yaml

all: lint start_geth test demo stop_geth

build: build/morra.ala.mjs build/morra_while.ala.mjs

build/%.ala.mjs: morra/%.ala
	stack build
	stack exec -- alacrityc -o build $^

lint:
	@`npm bin`/eslint -c .eslintrc.yaml "morra/**/*.mjs"

start_geth:
	@../../scripts/ethereum-devnet/run.sh

stop_geth:
	@killall geth

test: build
	@node --experimental-modules morra/spec/index.mjs

logs:
	@tail -f ../../_ethereum/geth-data/logs/testnet.log

demo: build
	@node --experimental-modules morra/demo-run-stdout.mjs
