CWD := $(CURDIR)

compile-tests:
	@rebar3 as test do clean, compile

check-runner-ltest: export CODE_PATH = _build/test/lib/*/ebin
check-runner-ltest: compile
	@clear
	@erl -pa ${CODE_PATH} -cwd "${CWD}" -listener ltest-listener -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-no-color: export CODE_PATH = _build/test/lib/*/ebin
check-runner-ltest-no-color: compile
	@clear
	@erl -pa ${CODE_PATH} \
	-cwd "${CWD}" -listener ltest-listener -color false -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-eunit: export CODE_PATH = _build/test/lib/*/ebin
check-runner-eunit: compile-tests
check-runner-eunit: compile
	@erl -pa ${CODE_PATH} -cwd "${CWD}" -listener eunit_progress -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell
