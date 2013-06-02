DEPS = ./deps
LFE_DIR = $(DEPS)/lfe
LFE_EBIN = $(LFE_DIR)/ebin
LFE = $(LFE_DIR)/bin/lfe
LFEC = $(LFE_DIR)/bin/lfec
ERL_LIBS = $(LFE_DIR):./
SOURCE_DIR = ./src
OUT_DIR = ./ebin
TEST_DIR = ./test
TEST_EBIN_DIR = ./.eunit
SANDBOX = ./sandbox

get-deps:
	rebar get-deps
	for DIR in $(wildcard $(DEPS)/*); do \
	cd $$DIR; git pull; cd - ; done

clean-ebin:
	-rm -f $(OUT_DIR)/*.beam

clean-eunit:
	-rm -rf $(TEST_EBIN_DIR)

compile: get-deps clean-ebin
	rebar compile

compile-tests: clean-eunit
	mkdir -p $(TEST_EBIN_DIR)
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_EBIN_DIR) $(TEST_DIR)/*_tests.lfe

shell: compile
	ERL_LIBS=$(ERL_LIBS) $(LFE) -pa $(TEST_EBIN_DIR)

clean: clean-ebin clean-eunit
	rebar clean

check: compile compile-tests
	@clear;
	rebar eunit skip_deps=true verbose=1

push:
	git push oubiwann master

push-all: push
	git push --all

commit: check
	@echo
	@echo "git commit ..."