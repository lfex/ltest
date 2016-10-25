compile:
	rebar3 compile

check:
	@rebar3 eunit

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

push:
	git push github master
	git push gitlab master

push-all: push
	git push github --tags
	git push gitlab --tags

publish:
	rebar3 as hexpm hex publish
