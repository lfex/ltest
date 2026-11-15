# ltest

[![Build](https://github.com/lfex/ltest/actions/workflows/cicd.yml/badge.svg?branch=release%2F0.14.x)](https://github.com/lfex/ltest/actions/workflows/cicd.yml?query=branch%3Arelease%2F0.14.x)
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

The API reference uses the published `ldoc` 0.1.1 Rebar project plugin. From
the repository root, run:

```sh
rebar3 ldoc --overview docs/overview.md --namespace hexpm
```

The provider compiles the default profile and refreshes its managed output at
`_build/default/ldoc/current/site/index.html`. `make docs` runs the same
provider command with the same overview and Hex namespace.

To keep a separate preview, pass a fresh output path whose parent already
exists. Existing paths are refused and left untouched:

```sh
make docs DOCS_OUTPUT=/path/to/fresh-reference
```

`make publish-dry-run` builds the reference in a unique private staging
directory, then runs one combined Hex package/docs dry-run with `--dry-run --yes`;
it removes only its own staging directory. `make publish` performs the
combined publication after the same isolated reference build. Use that target
only after release approval.

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
maintained `release/0.14.x` branch; the Hex badge remains the live publication
version.

## License

BSD 3-Clause License. See [`LICENSE`](LICENSE).
