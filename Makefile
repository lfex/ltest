PROJECT = ltest
ROOT_DIR = $(shell pwd)
REBAR_PROFILE ?= dev
LFE = _build/$(REBAR_PROFILE)/lib/lfe/bin/lfe
REBAR3 ?= rebar3

default: compile

include priv/make/code.mk
include priv/make/docs.mk
include priv/make/test.mk

help:
	@echo "ltest Make targets:"
	@echo "  compile       Compile the application"
	@echo "  check-native  Compile, run unfiltered EUnit, and run ltest with and without color"
	@echo "  check         Run the existing Github-source EUnit check"
	@echo "  docs          Build a fresh local ldoc preview (DOCGEN=... to select the CLI)"
	@echo "  publish-dry-run  Build docs and dry-run the combined Hex package/docs upload"
