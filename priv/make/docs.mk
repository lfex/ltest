# ldoc owns compilation and refreshes its managed default output.
DOCS_OVERVIEW := $(ROOT_DIR)/docs/overview.md
DOCS_PARENT := $(shell dirname -- "$(DOCS_OUTPUT)")

.PHONY: docs publish publish-dry-run

docs:
ifeq ($(strip $(DOCS_OUTPUT)),)
	@echo "Building managed ldoc reference at $(ROOT_DIR)/_build/default/ldoc/current/site"
	@"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm
else
	@test -d "$(DOCS_PARENT)" || { echo "docs output parent must already exist: $(DOCS_PARENT)" >&2; exit 1; }
	@test ! -e "$(DOCS_OUTPUT)" || { echo "docs output already exists; choose a fresh DOCS_OUTPUT: $(DOCS_OUTPUT)" >&2; exit 1; }
	@echo "Building ldoc reference at $(DOCS_OUTPUT)"
	@"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$(DOCS_OUTPUT)"
endif

publish-dry-run:
	@set -eu; \
	parent="$(ROOT_DIR)/_build"; \
	mkdir -p "$$parent"; \
	stage="$$(mktemp -d "$$parent/ltest-docs-dry-run.XXXXXX")"; \
	trap 'rm -rf -- "$$stage"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$$stage/reference"; \
	"$(REBAR3)" hex publish --doc-dir "$$stage/reference/site" --dry-run --yes

publish:
	@set -eu; \
	parent="$(ROOT_DIR)/_build"; \
	mkdir -p "$$parent"; \
	stage="$$(mktemp -d "$$parent/ltest-docs-publish.XXXXXX")"; \
	trap 'rm -rf -- "$$stage"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	"$(REBAR3)" ldoc --overview "$(DOCS_OVERVIEW)" --namespace hexpm --output "$$stage/reference"; \
	"$(REBAR3)" hex publish --doc-dir "$$stage/reference/site"
