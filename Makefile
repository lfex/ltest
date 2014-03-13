DEPS = ./deps
BIN_DIR=./bin
EXPM=$(BIN_DIR)/expm
PROJECT=lfeunit
LIB=$(PROJECT)
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
FINISH=-run init stop -noshell

get-version:
	@echo
	@echo -n package.exs: ''
	@grep version package.exs |awk '{print $$2}'|sed -e 's/,//g'
	@echo -n app.src: ''
	@erl -eval 'io:format("~p~n", [ \
		proplists:get_value(vsn,element(3,element(2,hd(element(3, \
		erl_eval:exprs(element(2, erl_parse:parse_exprs(element(2, \
		erl_scan:string("Data = " ++ binary_to_list(element(2, \
		file:read_file("src/$(LIB).app.src"))))))), []))))))])' \
		$(FINISH)

# Note that this make target expects to be used like so:
#	$ ERL_LIB=some/path make get-install-dir
#
# Which would give the following result:
#	some/path/lfe-rackspace-1.0.0
#
get-install-dir:
	@echo $(ERL_LIB)/$(PROJECT)-$(shell make get-version)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(EXPM): $(BIN_DIR)
	curl -o $(EXPM) http://expm.co/__download__/expm
	chmod +x $(EXPM)

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

compile-no-deps: clean-ebin
	rebar compile skip_deps=true

compile-tests: clean-eunit
	mkdir -p $(TEST_EBIN_DIR)
	ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_EBIN_DIR) $(TEST_DIR)/*[_-]tests.lfe

shell: compile
	@clear
	ERL_LIBS=$(ERL_LIBS) $(LFE) -pa $(TEST_EBIN_DIR)

shell-no-deps: compile-no-deps
	@clear
	ERL_LIBS=$(ERL_LIBS) $(LFE) -pa $(TEST_EBIN_DIR)

clean: clean-ebin clean-eunit
	rebar clean

check: compile compile-tests
	@clear
	rebar eunit skip_deps=true verbose=1

check-no-deps: compile-no-deps compile-tests
	@clear;
	@rebar eunit verbose=1 skip_deps=true

# Note that this make target expects to be used like so:
#	$ ERL_LIB=some/path make install
#
install: INSTALLDIR=$(shell make get-install-dir)
install: compile
	if [ "$$ERL_LIB" != "" ]; \
	then mkdir -p $(INSTALLDIR)/$(EBIN); \
		mkdir -p $(INSTALLDIR)/$(SRC); \
		cp -pPR $(EBIN) $(INSTALLDIR); \
		cp -pPR $(SRC) $(INSTALLDIR); \
	else \
		echo "ERROR: No 'ERL_LIB' value is set in the env." \
		&& exit 1; \
	fi

push-all:
	git push --all
	git push upstream --all
	git push --tags
	git push upstream --tags

upload: $(EXPM)
	$(EXPM) publish
