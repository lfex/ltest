PROJECT = ltest
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
REBAR_PROFILE ?= dev
LFE = _build/$(REBAR_PROFILE)/lib/lfe/bin/lfe
REBAR3 = PATH=.:$(PATH) rebar3

include priv/make/code.mk
include priv/make/docs.mk
include priv/make/test.mk
