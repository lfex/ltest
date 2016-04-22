DOCS_DIR = $(ROOT_DIR)/docs
GUIDE_DIR = $(DOCS_DIR)/user-guide
GUIDE_BUILD_DIR = $(GUIDE_DIR)/build
DOCS_PROD_DIR = $(DOCS_DIR)/master
API_PROD_DIR = $(DOCS_PROD_DIR)/current/api
GUIDE_PROD_DIR = $(DOCS_PROD_DIR)/current/user-guide
SLATE_GIT_HACK = $(DOCS_DIR)/.git
LOCAL_DOCS_HOST = localhost
LOCAL_DOCS_PORT = 5099

$(SLATE_GIT_HACK):
	@ln -s $(ROOT_DIR)/.git $(DOCS_DIR)

docs-setup:
	@echo "\nInstalling and setting up dependencies ..."
	@cd $(DOCS_DIR) && bundle install

docs-clean:
	@echo "\nCleaning build directories ..."
	@rm -rf $(GUIDE_BUILD_DIR) $(API_PROD_DIR) $(GUIDE_PROD_DIR)

docs-lodox:
	@echo
	@rebar3 lfe lodox

docs-slate:
	@echo
	@cd $(GUIDE_DIR) && bundle exec middleman build --clean
	@mkdir $(GUIDE_PROD_DIR)
	@cp -r $(GUIDE_BUILD_DIR)/* $(GUIDE_PROD_DIR)/

docs: clean compile docs-clean $(SLATE_GIT_HACK)
	@echo "\nBuilding docs ...\n"
	@make docs-lodox
	@make docs-slate

devdocs: docs
	@echo
	@echo "Running docs server on http://$(LOCAL_DOCS_HOST):$(LOCAL_DOCS_PORT) ... (To quit, hit ^c twice)"
	@echo
	@erl -s inets -noshell -eval 'inets:start(httpd,[{server_name,"devdocs"},{document_root, "$(DOCS_PROD_DIR)"},{server_root, "$(DOCS_PROD_DIR)"},{port, $(LOCAL_DOCS_PORT)},{mime_types,[{"html","text/html"},{"htm","text/html"},{"js","text/javascript"},{"css","text/css"},{"gif","image/gif"},{"jpg","image/jpeg"},{"jpeg","image/jpeg"},{"png","image/png"}]}]).'

setup-temp-repo: $(SLATE_GIT_HACK)
	@echo "\nSetting up temporary git repos for gh-pages ...\n"
	@rm -rf $(DOCS_PROD_DIR)/.git $(DOCS_PROD_DIR)/*/.git
	@cd $(DOCS_PROD_DIR) && git init
	@cd $(DOCS_PROD_DIR) && git add * > /dev/null
	@cd $(DOCS_PROD_DIR) && git commit -a -m "Generated content." > /dev/null

teardown-temp-repo:
	@echo "\nTearing down temporary gh-pages repos ..."
	@rm $(DOCS_DIR)/.git $(GUIDE_DIR)/Gemfile.lock
	@rm -rf $(DOCS_PROD_DIR)/.git $(DOCS_PROD_DIR)/*/.git

publish-docs: docs setup-temp-repo
	@echo "\nPublishing docs ...\n"
	@cd $(DOCS_PROD_DIR) && git push -f $(REPO) master:gh-pages
	@make teardown-temp-repo

.PHONY: docs
