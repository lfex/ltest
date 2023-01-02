CWD := $(CURDIR)

compile-tests:
	@$(REBAR3) as test do clean, compile

check-runner-ltest: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
check-runner-ltest: compile-tests
	@erl -pa ${CODE_PATH} -eval \
	"case ltest:all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-old: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
check-runner-ltest-old: compile-tests
	@erl -pa ${CODE_PATH} -cwd "${CWD}" -listener ltest-listener -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-no-color: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
check-runner-ltest-no-color: compile-tests
	@erl -pa ${CODE_PATH} -eval \
	"case ltest:all(#{color => false}) of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-no-color-old: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
check-runner-ltest-no-color-old: compile-tests
	@erl -pa ${CODE_PATH} \
	-cwd "${CWD}" -listener ltest-listener -color false -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-eunit: export CODE_PATH = _build/test/lib/*/ebin _build/test/lib/*/ebin _build/test/plugins/*/ebin
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
	$(REBAR3) as test eunit

check-gitlab:
	@echo
	@echo "=================================="
	@echo "Running tests using Gitlab Sources ..."
	@echo "=================================="
	@echo
	$(REBAR3) as gitlab eunit

check-hexpm: clean
	@echo
	@echo "==================================="
	@echo "Running tests using Hex.pm Packages ..."
	@echo "==================================="
	@echo
	@$(REBAR3) as hexpm lfe clean
	$(REBAR3) as hexpm eunit

check-all: check check-gitlab check-hexpm

travis:
	@if [ "$(REBAR_BUILD)" = "github" ]; then make build-github; make check; fi;
	@if [ "$(REBAR_BUILD)" = "gitlab" ]; then make build-gitlab; make check-gitlab; fi;
	@if [ "$(REBAR_BUILD)" = "hexpm" ]; then make build-hexpm; make check-hexpm; fi;
