compile:
	rebar3 compile

check:
	@rebar3 as test eunit

repl:
	@rebar3 as dev compile
	@$(LFE) -pa `rebar3 as dev path -s " -pa "`

shell:
	@rebar3 shell

clean:
	@rebar3 clean
	@rm -rf ebin/* _build/default/lib/$(PROJECT)

clean-all: clean
	@rebar3 as dev lfe clean
