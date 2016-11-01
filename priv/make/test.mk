CWD := $(CURDIR)

compile-tests:
	@$(REBAR3) as test do clean, compile

check-runner-ltest: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
check-runner-ltest: compile-tests
	@clear
	@erl -pa ${CODE_PATH} -cwd "${CWD}" -listener ltest-listener -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-no-color: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin
check-runner-ltest-no-color: compile-tests
	@clear
	@erl -pa ${CODE_PATH} \
	-cwd "${CWD}" -listener ltest-listener -color false -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-eunit: export CODE_PATH = _build/test/lib/*/ebin _build/default/lib/*/ebin
check-runner-eunit: compile-tests
	@erl -pa ${CODE_PATH} -cwd "${CWD}" -listener eunit_progress -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check:
	@echo
	@echo "=================================="
	@echo "Running tests using Github Sources ..."
	@echo "=================================="
	@echo
	$(REBAR3) as test lfe test

check-gitlab:
	@echo
	@echo "=================================="
	@echo "Running tests using Gitlab Sources ..."
	@echo "=================================="
	@echo
	$(REBAR3) as gitlab lfe test

check-hexpm: clean
	@echo
	@echo "==================================="
	@echo "Running tests using Hex.pm Packages ..."
	@echo "==================================="
	@echo
	@$(REBAR3) as hexpm lfe clean
	$(REBAR3) as hexpm lfe test

check-all: check check-gitlab check-hexpm

travis:
	@if [ "$(REBAR_BUILD)" = "github" ]; then make build-github; make check; fi;
	@if [ "$(REBAR_BUILD)" = "gitlab" ]; then make build-gitlab; make check-gitlab; fi;
	@if [ "$(REBAR_BUILD)" = "hexpm" ]; then make build-hexpm; make check-hexpm; fi;
