compile:
	@$(REBAR3) compile

repl:
	@$(REBAR3) lfe repl

shell:
	@$(REBAR3) shell

clean:
	@rm -rf rebar3.crashdump \
	_build/default/*/$(PROJECT) \
	_build/repl/*/$(PROJECT)

clean-all: clean
	@rm -rf \
	./_build

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags

publish:
	rebar3 publish
