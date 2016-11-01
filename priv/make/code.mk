compile:
	$(REBAR3) compile

repl:
	$(REBAR3) as $(REBAR_PROFILE) compile
	$(LFE) -pa `$(REBAR3) as $(REBAR_PROFILE) path -s " -pa "`

shell:
	@$(REBAR3) shell

clean:
	@$(REBAR3) clean
	@rm -rf ebin/* _build/default/lib/$(PROJECT) rebar.lock \
		rebar3.crashdump \
		$(HOME)/.cache/rebar3/hex/default/packages/*

clean-all: clean
	@$(REBAR3) as dev lfe clean

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags

build-github: clean
	@echo
	@echo "============================="
	@echo "Building using Github Sources ..."
	@echo "============================="
	@echo
	$(REBAR3) compile
	@$(REBAR3) lock

build-gitlab: clean
	@echo
	@echo "============================="
	@echo "Building using Gitlab Sources ..."
	@echo "============================="
	@echo
	$(REBAR3) as gitlab compile
	@$(REBAR3) as gitlab lock

build-hexpm: clean
	@echo
	@echo "=============================="
	@echo "Building using Hex.pm Packages ..."
	@echo "=============================="
	@echo
	$(REBAR3) as hexpm compile
	@$(REBAR3) as hexpm lock

build-all: build-github build-gitlab build-hexpm

publish: clean
	$(REBAR3) as hexpm hex publish

setup-rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod +x rebar3

setup-hexpm: REBAR_HOME_CONFIG = ~/.config/rebar3
setup-hexpm:
	mkdir -p $(REBAR_HOME_CONFIG)
	echo "{plugins, [rebar3_hex]}." > $(REBAR_HOME_CONFIG)/rebar.config
