# ldoc writes a complete preview root; Hex consumes its site/ subdirectory.
DOCGEN ?= ldoc
DOCS_OUTPUT := $(if $(DOCS_OUTPUT),$(DOCS_OUTPUT),$(ROOT_DIR)/.ldoc-preview-$(shell date +%s)-$(shell od -An -N8 -tx1 /dev/urandom | tr -d ' \n'))
DOCS_PARENT := $(shell dirname -- "$(DOCS_OUTPUT)")
DOCS_SITE = $(DOCS_OUTPUT)/site

.PHONY: docs publish publish-dry-run

docs: compile
	@test -d "$(DOCS_PARENT)" || { echo "docs output parent must already exist: $(DOCS_PARENT)" >&2; exit 1; }
	@test ! -e "$(DOCS_OUTPUT)" || { echo "docs output already exists; choose a fresh DOCS_OUTPUT: $(DOCS_OUTPUT)" >&2; exit 1; }
	@echo "Building ldoc preview at $(DOCS_OUTPUT)"
	@"$(DOCGEN)" build "$(ROOT_DIR)" \
	  --overview "$(ROOT_DIR)/docs/overview.md" \
	  --namespace hexpm \
	  --output "$(DOCS_OUTPUT)"

publish-dry-run: docs
	@"$(REBAR3)" hex publish --doc-dir "$(DOCS_SITE)" --dry-run --yes

publish: docs
	@"$(REBAR3)" hex publish --doc-dir "$(DOCS_SITE)"
