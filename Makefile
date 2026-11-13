PROJECT = ltest
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
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
