# Requirements: you need to install the solidity compiler, version 0.5.2 or later

# make VERBOSE nonempty to see raw commands (or provide on command line)
ifndef VERBOSE
VERBOSE:=
endif

# use SHOW to inform user of commands
SHOW:=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE:=$(if $(VERBOSE),,@)

all: contract lint

.PHONY: all contract test lint run_ethereum

AUTO_CONTRACT_OUTPUTS=build/contract.mjs
ALAB=../../hs/alacrity/ala/_build

contract: $(AUTO_CONTRACT_OUTPUTS)
$(AUTO_CONTRACT_OUTPUTS): $(ALAB)/ALAContract.bin $(ALAB)/ALAContract.abi
	$(HIDE) mkdir -p build
	$(HIDE) (echo "export const contractAbi = `cat $(ALAB)/ALAContract.abi`;"; \
	echo; echo "export const contractCode = \"0x`cat $(ALAB)/ALAContract.bin`\";"; \
	) > $@

lint:
	$(HIDE) `npm bin`/eslint -c .eslintrc.yaml rps/**/*.mjs

run_ethereum:
	$(HIDE) ../../scripts/ethereum-devnet/run.sh

test:
	$(HIDE) node --experimental-modules rps/spec
