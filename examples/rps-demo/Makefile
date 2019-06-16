# Makefile for Alacrity Rock-Paper-Scissors demo
#
# Requirements: you need to install the solidity compiler, version 0.5.2 or later
# (see dapp-contract.sol for the authoritative version we need).
#

# make VERBOSE nonempty to see raw commands (or provide on command line)
ifndef VERBOSE
VERBOSE:=
endif

# use SHOW to inform user of commands
SHOW:=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE:=$(if $(VERBOSE),,@)

# Our applications recognize this as the top directory for the project, and look for files there
# at runtime, e.g. for configuration.
export ALACRITY_HOME:=$(shell cd ../.. ; pwd)

BUILD_DIR:=$(ALACRITY_HOME)/_build/examples/rps-demo

all: build_all

.PHONY: all build_all force contract start_nginx restart_nginx stop_nginx

build_all: contract

MANUAL_CONTRACT_OUTPUTS:=build/dapp-contract.mjs $(BUILD_DIR)/RockPaperScissors.abi $(BUILD_DIR)/RockPaperScissors.bin $(BUILD_DIR)/RockPaperScissorsFactory.abi $(BUILD_DIR)/RockPaperScissorsFactory.bin
manual_contract: $(MANUAL_CONTRACT_OUTPUTS)
$(MANUAL_CONTRACT_OUTPUTS): dapp-contract.sol
	$(HIDE) mkdir -p build $(BUILD_DIR)
	$(HIDE) solc --bin --abi -o $(BUILD_DIR) --overwrite $< > $(BUILD_DIR)/solc.out
	$(HIDE) (echo "export const contractAbi = `cat $(BUILD_DIR)/RockPaperScissors.abi`;"; \
	echo; echo "export const contractFactoryAbi = `cat $(BUILD_DIR)/RockPaperScissorsFactory.abi`;"; \
	echo; echo "export const contractFactoryCode = \"0x`cat $(BUILD_DIR)/RockPaperScissorsFactory.bin`\";"; \
	) > $@
# To extract function signatures, add the --hashes to solc and use:
#	echo; echo "const rpsSignatures = {`sed -e 's/^\(.*\): \(.*\)(.*)/"\2": "\1",/' $(BUILD_DIR)/RockPaperScissors.signatures`};" ; \
#	echo; echo "const rpsFactorySignatures = {`sed -e 's/^\(.*\): \(.*\)(.*)/"\2": "\1"/' $(BUILD_DIR)/RockPaperScissorsFactory.signatures`};" ; \

AUTO_CONTRACT_OUTPUTS:=build/rps-ala-contract.mjs
ALAB=../../hs/alacrity/ala/_build
auto_contract: $(AUTO_CONTRACT_OUTPUTS)
$(AUTO_CONTRACT_OUTPUTS): $(ALAB)/ALAFactory.bin $(ALAB)/ALAFactory.abi $(ALAB)/ALAContract.abi
	$(HIDE) mkdir -p build $(BUILD_DIR)
	$(HIDE) solc --bin --abi -o $(BUILD_DIR) --overwrite $< > $(BUILD_DIR)/solc.out
	$(HIDE) (echo "export const contractAbi = `cat $(ALAB)/ALAContract.abi`;"; \
	echo; echo "export const contractFactoryAbi = `cat $(ALAB)/ALAFactory.abi`;"; \
	echo; echo "export const contractFactoryCode = \"0x`cat $(ALAB)/ALAFactory.bin`\";"; \
	) > $@

start_nginx:
	$(HIDE) ../nginx/start.sh

stop_nginx:
	$(HIDE) ../nginx/stop.sh

restart_nginx:
	$(HIDE) ../nginx/stop.sh ; ../nginx/start.sh

# Install web3 at the precise version we want, as well as other dependencies.
# https://github.com/ethereum/wiki/wiki/JavaScript-API
# https://www.npmjs.com/package/node-localstorage
# https://jasmine.github.io/tutorials/your_first_suite
npm_install:
	$(HIDE) npm install web3@0.20.7 node-localstorage@1.3.1 jasmine@3.4.0

test:
	$(HIDE) node --experimental-modules test.js -- test

deploy:
	$(HIDE) node --experimental-modules test.js -- deploy

lint:
	$(HIDE) ./node_modules/eslint/bin/eslint.js *.mjs

run_ethereum:
	$(HIDE) ../../scripts/ethereum-devnet/run.sh
