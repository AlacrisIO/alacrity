.PHONY: all rps
all: rps

rps:
	cd examples/rps-auto && $(MAKE) all
