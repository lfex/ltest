# ltest

[![Build](https://github.com/lfex/ltest/actions/workflows/cicd.yml/badge.svg)](https://github.com/lfex/ltest/actions/workflows/cicd.yml)
[![LFE 2.2.2](https://img.shields.io/badge/LFE-2.2.2-blue.svg)](https://github.com/lfe/lfe)
[![OTP 27–29](https://img.shields.io/badge/OTP-27%E2%80%9329-blue.svg)](./.github/workflows/cicd.yml)
[![Hex version](https://img.shields.io/hexpm/v/ltest.svg)](https://hex.pm/packages/ltest)

`ltest` runs LFE tests through EUnit and separates unit, system, and integration
modules using marker behaviors. It provides LFE assertion macros, lazy EUnit
test objects, skip reporting, and a text reporter.

This checkout is an unpublished source candidate whose compiled application
metadata remains `0.13.11`. Published Hex `0.13.11` predates this candidate
and does not contain its documentation changes or later helper APIs. The Hex
badge reports the published package version, not the contents of this checkout.

## Quick start

To use the published Hex `0.13.11` package, add this configuration to an LFE
project:

```erlang
{deps, [{lfe, "2.2.2"}]}.
{project_plugins, [{rebar3_lfe, "0.5.8"}]}.
{profiles, [
  {test, [
    {deps, [{ltest, "0.13.11"}]}
  ]}
]}.
```

The root project needs its own LFE dependency and LFE provider plugin as well
as the ltest test dependency. Rebar does not inherit a dependency's
`project_plugins` entry. This configuration describes the published package.
A fresh packaged-consumer compile for this source candidate has not been
verified, so these instructions are not evidence that the candidate installs
successfully. The repository's source checks use `make check-native`. Keep
the Rebar/LFE project checkout path free of spaces for the current
compiler-hook workflow; the CLI paths below remain quoted.

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
source, prefix, and cache remain under `work_root`. The current Rebar/LFE
compiler-hook workflow expects a space-free project checkout path; the CLI
arguments above are still quoted.

The pinned ldoc build compiles the application and records its compiled
identity; the output directory name does not set a version. The overview
provides the header macro reference because included macros have no native BEAM
docstring carrier. Native LFE functions in this package do not record source
filenames, so this recipe does not claim source links. The compiled candidate
identity currently remains `0.13.11`. This recipe renders documentation; it
does not publish the package or verify a fresh packaged-consumer compile.

The checked-in `docs/ltest.txt` is a legacy entrypoint; it points readers to
the current overview and generated native reference.

## Development checks

The repository's native check is:

```sh
make check-native
```

The checked workflow runs this command with OTP `27.3.4.19`, `28.5.0.8`, and
`29.1.2`, using Rebar3 `3.27.0`. These are tested versions, not an asserted
minimum or maximum support boundary.

## License

BSD 3-Clause License. See [`LICENSE`](LICENSE).
