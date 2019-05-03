# Makefile for Alacrity Rock-Paper-Scissors demo

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

CONTRACT_OUTPUTS=$(BUILD_DIR)/RockPaperScissors.abi $(BUILD_DIR)/RockPaperScissors.bin $(BUILD_DIR)/RockPaperScissorsFactory.abi $(BUILD_DIR)/RockPaperScissorsFactory.bin
contract: $(CONTRACT_OUTPUTS)
$(CONTRACT_OUTPUTS):
	$(HIDE) solc --bin --abi -o $(BUILD_DIR) --overwrite rock-paper-scissors.sol

start_nginx:
	$(HIDE) ../nginx/start.sh

stop_nginx:
	$(HIDE) ../nginx/stop.sh

restart_nginx:
	$(HIDE) ../nginx/stop.sh ; ../nginx/start.sh
