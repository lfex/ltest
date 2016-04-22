PROJECT = ltest
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE = _build/dev/lib/lfe/bin/lfe

include priv/make/code.mk
include priv/make/docs.mk
include priv/make/test.mk
