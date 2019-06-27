# Requirements: you need to install the solidity compiler, version 0.5.2 or later

all: contract lint

.PHONY: all contract test lint run_ethereum logs

contract:
	cd ../../hs/alacrity && stack test && cd ../../examples/rps-auto

lint:
	`npm bin`/eslint -c .eslintrc.yaml rps/*.mjs

run_ethereum:
	../../scripts/ethereum-devnet/run.sh

test:
	node --experimental-modules rps/spec/index.mjs

logs:
	tail -f ../../_ethereum/geth-data/logs/testnet.log
