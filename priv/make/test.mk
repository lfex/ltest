check-runner-ltest: compile
	@clear
	@erl -pa `rebar3 path -s " -pa "` \
	-cwd "`pwd`" -listener ltest-listener -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-ltest-no-color: compile
	@clear
	@erl -pa `rebar3 path -s " -pa "` \
	-cwd "`pwd`" -listener ltest-listener -color false -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

check-runner-eunit: compile
	@erl -pa `rebar3 path -s " -pa "` \
	-cwd "`pwd`" -listener eunit_progress -eval \
	"case 'ltest-runner':all() of ok -> halt(0); _ -> halt(127) end" \
	-noshell

