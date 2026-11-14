# ltest

[![Build](https://github.com/lfex/ltest/actions/workflows/cicd.yml/badge.svg?branch=codex%2Fltest-first-library)](https://github.com/lfex/ltest/actions/workflows/cicd.yml?query=branch%3Acodex%2Fltest-first-library)
[![LFE 2.2.2](https://img.shields.io/badge/LFE-2.2.2-blue.svg)](https://github.com/lfe/lfe)
[![OTP 27–29](https://img.shields.io/badge/OTP-27%E2%80%9329-blue.svg)](./.github/workflows/cicd.yml)
[![Hex version](https://img.shields.io/hexpm/v/ltest.svg)](https://hex.pm/packages/ltest)

`ltest` runs LFE tests through EUnit and separates unit, system, and integration
modules using marker behaviors. It provides LFE assertion macros, lazy EUnit
test objects, skip reporting, and a text reporter.

This source tree identifies the application as `0.14.0`. The Hex badge reports
the package version currently available from Hex.

## Quick start

For an LFE project using `ltest` `0.14.0`, add this root configuration:

```erlang
{deps, [{lfe, "2.2.2"}]}.
{project_plugins, [{rebar3_lfe, "0.5.8"}]}.
{profiles, [
  {test, [
    {deps, [{ltest, "0.14.0"}]},
    {erl_opts, [{src_dirs, ["src", "test"]}]}
  ]}
]}.
```

The root project needs its own LFE dependency and provider plugin for the `lfe`
command; Rebar does not inherit a dependency's `project_plugins` entry. The test
profile adds `test/` to the LFE source directories so `ltest` can discover the
compiled marker-behavior modules from the application's `ebin` directory.
Published `0.13.x` packages are historical; this recipe describes `0.14.0`.

The candidate's pre-publication consumer check uses the official
`rebar3 hex build` source archive as an ordinary Git dependency. That validates
the archive contents and consumer build; it is not a Hex install. The recipe
above uses the Hex dependency when version `0.14.0` is available. Run this
repository's source checks with `make check-native`.

In a test module, include the public macro header and mark the module's test
group:

```lisp
(defmodule calculator-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest adds-two-numbers
  (is-equal 5 (+ 2 3)))
```

Run a selected group with the LFE Rebar3 provider:

```sh
rebar3 as test lfe ltest
rebar3 as test lfe ltest -tintegration
rebar3 as test lfe ltest -tsystem
rebar3 as test lfe ltest -tall
```

The test type names are `unit`, `integration`, `system`, and `all`. The all
group runs unit, system, and integration tests in that order. Its return value
is the first non-`ok` group result; later groups still run.

The complete macro reference, behavior contracts, and source examples are in
[`docs/overview.md`](docs/overview.md). The native API reference includes the
exported helpers as well as the main runner functions.

## Test modules and names

Place test modules under `test/`, compiled as BEAM modules. Mark each module
with one of the behaviors `ltest-unit`, `ltest-system`, or
`ltest-integration`. These behaviors are marker interfaces with no callbacks;
`ltest` discovers modules from their behavior attributes, then runs the
selected BEAMs through EUnit.

`deftest NAME` defines a nullary `NAME_test/0`; `deftestgen NAME` defines a
nullary generator `NAME_test_/0`; and `deftestskip NAME` defines a nullary
`NAME_skip/0`. Export it explicitly or use `export all` for the custom reporter
to list it as skipped. The reporter
does not execute the function. Ordinary EUnit `_test` and `_test_` naming
still applies to tests that do not use these macros. The skip list recognizes
exported nullary names ending exactly in `_skip`.

LFE names may contain dashes. Test definition macros convert dashes to
underscores in generated Erlang function names. Fixture and generated-test
names should follow EUnit's naming and arity rules.

## Runner options and results

`ltest:default-opts/0` enables color, suite headers, and verbose output; it
selects `ltest-listener` and the `unit` test type. Public group `/1`
functions merge partial maps over these defaults and force their own group
type; `/0` uses the complete defaults. Groups return EUnit's
canonical result. `all/0,1` runs unit, system, and integration in that order
even after a failure, then returns the first non-`ok` result.

`run/0` selects the default unit group. `run/1` merges a partial map with the
defaults and dispatches its `test-type`; it maps EUnit `error` to
`{error, tests_failed}`, preserves other results, and returns an explanatory
`{error, Message}` for an unknown type. The EUnit option helpers require the
keys they read and do not merge defaults independently.

The built-in listener uses EUnit's `eunit_listener` callbacks. Its `init/1`
defaults omitted color to `true` and omitted test type to `unit`, preserving
explicit values. Set `color` to `false` to prevent the listener's standard
color wrappers from adding ANSI sequences.

## Regenerate the local API reference

The reference uses the public standalone ldoc command. Prerequisites are Git,
Make, Erlang/OTP with EDoc, and Rebar3. Run this block from an existing ltest
source checkout. It claims a unique caller-owned work root and keeps the ldoc
build cache, Rebar global configuration, and install prefix inside it; it
deliberately leaves the root in place for inspection.

```sh
set -eu
project="$(pwd -P)"
work_root="$(mktemp -d "${TMPDIR:-/tmp}/ltest-ldoc.XXXXXX")"
ldoc_source="$work_root/ldoc"
ldoc_prefix="$work_root/prefix"
export REBAR_CACHE_DIR="$work_root/rebar-cache"
export REBAR_GLOBAL_CONFIG_DIR="$work_root/rebar-global-config"
mkdir -p "$REBAR_CACHE_DIR" "$REBAR_GLOBAL_CONFIG_DIR"

git clone https://github.com/lfex/ldoc.git "$ldoc_source"
git -C "$ldoc_source" checkout --detach a3984487ca788f85e491f2956bab466393298413
make -C "$ldoc_source" check
make -C "$ldoc_source" install PREFIX="$ldoc_prefix"
"$ldoc_prefix/bin/ldoc" build "$project" \
  --output "$project/.ldoc-preview-1" \
  --overview "$project/docs/overview.md" \
  --namespace hexpm
```

The output parent is the existing project checkout and the output path must be
fresh. Use a new name such as `.ldoc-preview-2` on the next render; never
replace or clean a prior output as part of the recipe. The caller-owned ldoc
source, prefix, and cache remain under `work_root`. All project and output
paths in this recipe are quoted. Each render must use a fresh output root; the
recipe never removes or replaces a prior preview.

The pinned ldoc build compiles the application as `0.14.0`; the output
directory name does not set the version. The overview provides the header
macro reference because included macros have no native BEAM docstring carrier.
Native LFE functions in this package do not record source filenames, so this
recipe does not claim source links. The command renders documentation only.

For the repository Make workflow, install or select `ldoc` and run
`make docs DOCGEN=/path/to/ldoc`; Make prints the captured fresh preview root.
`make publish-dry-run DOCGEN=/path/to/ldoc` builds into one fresh root and always
runs the combined source-and-docs Hex dry-run with `--dry-run --yes`. Existing
preview roots are refused; Make never deletes previous output. The `make
publish DOCGEN=/path/to/ldoc` target performs one combined source-and-docs Hex
publication after docs build successfully. Use it only for an approved
release; the dry-run target never publishes.

The checked-in `docs/ltest.txt` is a legacy entrypoint; it points readers to
the current overview and generated native reference.

## Development checks

The repository's native check is:

```sh
make check-native
```

The checked workflow runs this command with Rebar3 `3.28.0` on OTP
`27.3.4.19`, `28.5.0.8`, and `29.1.2`. These are tested versions, not an
asserted minimum or maximum support boundary. Its build badge points at the
current candidate branch; the Hex badge remains the live publication version.

## License

BSD 3-Clause License. See [`LICENSE`](LICENSE).
