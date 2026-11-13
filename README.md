# ltest

[![Build](https://github.com/lfex/ltest/actions/workflows/cicd.yml/badge.svg)](https://github.com/lfex/ltest/actions/workflows/cicd.yml)
[![LFE 2.2.2](https://img.shields.io/badge/LFE-2.2.2-blue.svg)](https://github.com/lfe/lfe)
[![OTP 27–29](https://img.shields.io/badge/OTP-27%E2%80%9329-blue.svg)](https://github.com/lfex/ltest/blob/main/.github/workflows/cicd.yml)
[![Hex version](https://img.shields.io/hexpm/v/ltest.svg)](https://hex.pm/packages/ltest)

`ltest` runs LFE tests through EUnit and separates unit, system, and integration
modules using marker behaviors. It provides LFE assertion macros, lazy EUnit
test objects, skip reporting, and a text reporter.

The checked source tree and current published package are version `0.13.11`.
This documentation describes that API surface; it does not announce a new
release.

## Quick start

Add the published package to the test profile of an LFE project:

```erlang
{deps, [{lfe, "2.2.2"}]}.
{project_plugins, [{rebar3_lfe, "0.5.8"}]}.
{profiles, [
  {test, [
    {deps, [{ltest, "0.13.11"}]}
  ]}
]}.
```

The root project needs the LFE provider and LFE dependency as well as the ltest
test dependency. The `project_plugins` entry registers the provider in the
consumer project; the plugin declared by a dependency is not inherited. A
fresh packaged-consumer compile for this source candidate is still pending its
Rebar compiler-hook handoff, so this configuration is not evidence of a
successful package install. The repository's source checks are run with
`make check-native`.

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
`NAME_skip/0` that the custom reporter lists without executing. Ordinary EUnit
`_test` and `_test_` naming still applies to tests that do not use these
macros. The custom skip list recognizes exported nullary names ending exactly
in `_skip`.

LFE names may contain dashes. Test definition macros convert dashes to
underscores in generated Erlang function names. Fixture and generated-test
names should follow EUnit's naming and arity rules.

## Runner options and results

`ltest:default-opts/0` returns options for color, suite headers, listener,
test type, and verbosity. Public group functions merge supplied options with
those defaults. `ltest:unit/0,1`, `system/0,1`, `integration/0,1`, and `all/0,1`
return the underlying EUnit result. `ltest:run/0,1` is the provider entrypoint:
it returns `ok` on success, `{error, tests_failed}` for EUnit's `error`, and
preserves other `{error, Reason}` results.

The built-in listener uses EUnit's `eunit_listener` callbacks. Its empty and
partial option lists default to color enabled and test type `unit`; explicit
values are retained. Set `color` to `false` to omit ANSI color sequences.

## Regenerate the local API reference

The reference uses the public standalone ldoc command. Build it from the
reviewed source commit, install it into a project-owned prefix, and use a fresh
output directory on each run:

```sh
git clone https://github.com/lfex/ldoc.git /tmp/ldoc-public
git -C /tmp/ldoc-public checkout a3984487ca788f85e491f2956bab466393298413
make -C /tmp/ldoc-public check
make -C /tmp/ldoc-public install PREFIX=/tmp/ldoc-prefix
/tmp/ldoc-prefix/bin/ldoc build "$PWD" \
  --output "$PWD/.ldoc-preview-1" \
  --overview docs/overview.md \
  --namespace hexpm
```

The output parent must already exist and the output path must not. For another
render, use a new name such as `.ldoc-preview-2`; ldoc retains the complete
staged package and native snapshot in that output. The overview provides the
header macro reference because included macros have no native BEAM docstring
carrier. The release identity displayed by ldoc comes from the compiled
application, not the output folder name. The checked source application is
still `0.13.11`. Native LFE functions in this package do not record source
filenames, so this recipe does not claim source links.

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
