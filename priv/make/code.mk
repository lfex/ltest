compile:
	@$(REBAR3) compile

repl:
	@$(REBAR3) lfe repl

shell:
	@$(REBAR3) shell

clean:
	@rm -rf rebar3.crashdump rebar.lock \
	_build/default/*/$(PROJECT) \
	_build/repl/*/$(PROJECT)

clean-all: clean
	@rm -rf \
	./_build \
	$(HOME)/.cache/rebar3/hex/default/packages/* \
	$(HOME)/.cache/rebar3/*/$(PROJ)

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags
