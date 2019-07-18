.PHONY: all build test lint start_geth stop_geth logs

export STACK_YAML=../../hs/alacrity/stack.yaml

all: lint start_geth test demo stop_geth

build: build/rps.ala.mjs

while: build build/rps_while.ala.mjs

build/%.ala.mjs: rps/%.ala
	stack build --allow-different-user
	stack exec -- alacrityc -o build $^

lint:
	@`npm bin`/eslint -c .eslintrc.yaml "rps/**/*.mjs"

start_geth:
	@../../scripts/ethereum-devnet/run.sh

stop_geth:
	@killall geth

test: build/rps.ala.mjs
	@node --experimental-modules rps/spec/index.mjs

logs:
	@tail -f ../../_ethereum/geth-data/logs/testnet.log

demo: build/rps.ala.mjs
	@node --experimental-modules rps/demo-run-stdout.mjs
