# Makefile for the nightly machine
# Will not work on other systems

nightly:
	bash infra/nightly.sh

.PHONY: nightly
