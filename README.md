# ltest

[![Build](https://github.com/lfex/ltest/actions/workflows/cicd.yml/badge.svg?branch=release%2F0.14.x)](https://github.com/lfex/ltest/actions/workflows/cicd.yml?query=branch%3Arelease%2F0.14.x)
[![LFE 2.2.2](https://img.shields.io/badge/LFE-2.2.2-blue.svg)](https://github.com/lfe/lfe)
[![OTP 27–29](https://img.shields.io/badge/OTP-27%E2%80%9329-blue.svg)](./.github/workflows/cicd.yml)
[![Hex version](https://img.shields.io/hexpm/v/ltest.svg)](https://hex.pm/packages/ltest)
[![Tags](https://img.shields.io/github/v/tag/lfex/ltest.svg?sort=semver)](https://github.com/lfex/ltest/tags)
[![Downloads][hex-downloads]][hex-package]
[![HexDocs][hex-docs-badge]][hex-docs]

[![ltest logo][ltest-logo]][ltest-logo-large]

*A Unit, Integration, and System Tests Framework for LFE*

## Contents

- [Introduction](#introduction)
- [Dependencies](#dependencies)
- [EUnit compatibility](#eunit-compatibility)
- [Features](#features)
- [Quick start](#quick-start)
- [Test modules and names](#test-modules-and-names)
- [Creating unit tests](#creating-unit-tests)
- [Generators, named tests and test sets](#generators-named-tests-and-test-sets)
- [Fixtures](#fixtures)
- [Skipping tests](#skipping-tests)
- [Runner options and results](#runner-options-and-results)
- [The LFE test runner](#the-lfe-test-runner)
- [Dogfood](#dogfood)
- [Development checks](#development-checks)
- [License](#license)

## Introduction

`ltest` runs LFE tests through EUnit and separates unit, system, and integration
modules using marker behaviors. It provides LFE assertion macros, lazy EUnit
test objects, skip reporting, and a text reporter.

This source tree identifies the application as `0.14.0`. The Hex badge reports
the package version currently available from Hex.

The testing DSL is inspired by Clojure's testing library. Its macros give
EUnit assertions a Lispy form, while the custom listener makes passing,
failing and skipped tests easy to distinguish in coloured output.

## Dependencies

Use Rebar3 with LFE `2.2.2` and the `rebar3_lfe` plugin shown below. The
current development checks use Rebar3 `3.28.0` on OTP 27, 28 and 29; the exact
versions are listed under [Development checks](#development-checks).

For historical projects, ltest `0.11.0` introduced LFE 2.0 support. The
`0.7.0` line switched to Rebar3; `0.6.3` is the documented legacy release
for projects still using Rebar2.

## EUnit compatibility

ltest test macros produce ordinary EUnit tests and test objects. Compiled
tests can be run from Erlang or LFE with EUnit's standard listener, or with
the ltest listener. For example, the quick-start module below can be run
with `eunit:test('calculator-tests', [verbose])` from Erlang, or as part of:

```sh
rebar3 as test eunit
```

The ltest runner adds marker-based unit/integration/system selection and
reports explicitly exported skipped tests. A plain EUnit run does not list
those manually skipped functions.

## Features

- `deftest` defines ordinary EUnit tests with immediate assertion macros.
- `deftestgen` defines generators, including EUnit setup/teardown fixtures.
- `deftestskip` marks work to be skipped and reported by the ltest listener.
- Lists group test objects into sets, which may be nested.
- Named tuples give test objects readable descriptions.
- Marker behaviors separate unit, integration and system test modules.
- A custom listener reports results, skipped tests, elapsed time and failures
  with optional colour.

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

Run the provider's default selection with:

```sh
rebar3 as test lfe ltest
```

Library selectors are implemented in this candidate. The cached
`rebar3_lfe` 0.5.8 provider source exposes `--suite` and function-oriented
`--test`, but has no module `-m` option. Its command-line selection contract
is not delivered or certified here; Slice02 owns that provider work. Use the
public library API below for the selectors described in this document.

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

A useful starting layout is one test module per source module, splitting
larger suites further when that makes the tests easier to navigate:

```text
src/myproj-base.lfe
src/myproj-util.lfe
test/myproj-base-tests.lfe
test/myproj-util-tests.lfe
```

The filename must match the declared module: `myproj-base-tests.lfe`
declares `(defmodule myproj-base-tests ...)`. A `-tests` suffix is a naming
convention; ltest selects modules by their compiled behavior attributes.
Keep test modules in `test/` with the test-profile configuration above.
Simply adding tests to an unmarked application module does not put that
module into one of ltest's groups.

Choose `(behaviour ltest-unit)`, `(behaviour ltest-integration)` or
`(behaviour ltest-system)` in each test module according to its role.
See the project's own [test directory](test/) for examples of each group.

## Creating unit tests

Include `ltest-macros.lfe` to use the assertion and definition macros.
`deftest` supplies the empty argument list, EUnit suffix and export for you:
`(deftest adds-two-numbers ...)` defines `adds_two_numbers_test/0`. If you
write a test with `defun` instead, give it a nullary `_test` name and export
it for EUnit to discover.

Save this complete example as `test/arithmetic-tests.lfe`:

```lisp
(defmodule arithmetic-tests
  (behaviour ltest-unit)
  (export (pending_division_skip 0)))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest true-and-false
  (is 'true)
  (is-not 'false))

(deftest equality
  (is-equal 5 (+ 2 3))
  (is-not-equal 1 1.0))

(deftest pattern-matching
  (is-match (tuple 'ok _) (tuple 'ok 42)))

(deftestskip pending-division
  (error 'not-implemented-yet))
```

`is-equal` uses exact term equality, so `1` and `1.0` are different. The
[macro reference](docs/overview.md#assertion-and-test-object-reference) covers
all assertion families, including errors, exits, throws and pattern matches.

## Generators, named tests and test sets

A `deftest` body executes assertions immediately. A `deftestgen` body returns
EUnit test objects to be executed later; use the delayed assertion forms
ending in `*` when constructing those objects. Lists form test sets, nested
lists form nested sets, and a `{Description, Test}` tuple names a test or set.

Save this as `test/arithmetic-generator-tests.lfe`:

```lisp
(defmodule arithmetic-generator-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(deftestgen additions
  (list
    (is-equal* 4 (+ 2 2))
    (tuple "Nested additions"
      (list
        (is-equal* 6 (+ 3 3))
        (is-equal* 8 (+ 4 4))))))
```

Use delayed assertions inside the named tuple too. Returning the result of
an immediate assertion can produce a value instead of a valid EUnit test
object. `are*` is a documented exception to the delayed naming convention:
it substitutes rows and runs assertions immediately.

## Fixtures

EUnit fixtures arrange setup, test instantiation and teardown. ltest's
`defsetup`, `defteardown`, `deftestcase` and `deftestcases` helpers let you
write those parts in LFE. Define each case before using it in `deftestcases`.

Save this as `test/arithmetic-fixture-tests.lfe`:

```lisp
(defmodule arithmetic-fixture-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun set-up ()
  (tuple 1 2))

(defun tear-down (_pair)
  'ok)

(deftestcase addition-case (pair)
  (is-equal 3 (+ (element 1 pair) (element 2 pair))))

(deftestgen with-fixture
  (tuple 'setup
    (defsetup set-up)
    (defteardown tear-down)
    (deftestcases addition-case)))
```

Setup supplies the value passed to the test case and teardown. For a fixture
that acquires a resource, release it in teardown. The repository includes
[standard EUnit fixtures](test/ltest-fixture-tests.lfe) and
[fixture-case helper examples](test/ltest-fixturecase-tests.lfe), including
`foreach` cases.

## Skipping tests

The `pending-division` example above defines `pending_division_skip/0`.
It is explicitly exported so ltest can report it as skipped; its body is
never executed. `deftestskip` does not export the function for you.

EUnit's normal `_test` / `_test_` selection ignores these `_skip` functions.
The ltest listener finds and counts them separately. See the
[skipped-test example](test/ltest-skipped-tests.lfe) for a complete module.

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

Pass `modules` to select discovered test modules. `module` and `suite` are
compatibility aliases for the same module-name set. Names match the module
identity declared in an eligible discovered BEAM, not a filename, function,
regular expression, or wildcard. Values may be one atom, UTF-8 binary, or
Unicode character list, or a nonempty proper list of those scalar forms.
Each scalar may contain comma-separated names; ASCII space, tab, carriage
return, and newline are trimmed from token edges. Names are case-sensitive,
deduplicated, and sorted for alias comparison; test execution keeps the
existing discovery order. `all` is an ordinary module name. No selector keeps
the current run-all behavior; an explicit empty value or empty token is
invalid.

For example, an Erlang caller can select two modules with:

```erlang
ltest:unit(#{modules => [<<"calculator-tests">>, <<"support-tests">>]}).
ltest:run(#{'test-type' => all, suite => <<"calculator-tests,support-tests">>}).
```

The equivalent LFE call is:

```lisp
(ltest:unit #m(modules (list #"calculator-tests" #"support-tests")))
(ltest:run #m(test-type all suite #"calculator-tests,support-tests"))
```

When several aliases are supplied, their normalized sets must match exactly.
An invalid value returns `{error, {invalid_selector, Key, RawValue}}`; differing
aliases return `{error, {conflicting_selectors, Aliases}}`; names absent from
the requested group's discovered BEAMs return
`{error, {unmatched_modules, MissingNames}}`. Names are checked against BEAM
metadata without converting caller text to atoms. Every selector is validated
before EUnit runs: a partly matching request runs no tests, and `all` checks
the union of unit, system, and integration modules before starting its groups.
Empty selected groups succeed. Direct `unit/1`, `system/1`, and
`integration/1` calls always use their own group even if `test-type` disagrees.

This library API does not select individual test functions. `test` may be
absent or `undefined`; any other value returns
`{error, {unsupported_test_selector, RawValue}}` before EUnit runs. `all/0,1`
runs unit, system, and integration in that order even after a selected group
fails, then returns the first non-`ok` result. `run/1` maps only bare EUnit
`error` to `{error, tests_failed}` and preserves these tagged selector errors.

The built-in listener uses EUnit's `eunit_listener` callbacks. Its `init/1`
defaults omitted color to `true` and omitted test type to `unit`, preserving
explicit values. Set `color` to `false` to prevent the listener's standard
color wrappers from adding ANSI sequences.

## The LFE test runner

The custom listener adds coloured progress, readable assertion failures and
skip counts to EUnit's execution. These retained screenshots illustrate the
reporter's appearance in earlier releases; test inventories and timings vary.

Failing and skipped tests:

![Example failing test report][report-failing]
![Example failing test summary][summary-failing]

Passing tests:

![Example passing test report][report-passing]
![Example passing test summary][summary-passing]

Use the [runner options](#runner-options-and-results) to disable colour or
call the runner directly. The recommended command-line entrypoint is the
Rebar3 LFE provider shown in [Quick start](#quick-start).

## Dogfood

ltest tests itself with ltest. From this source checkout, run all three groups
through its custom listener:

```sh
make check-runner-ltest
```

The current unit summary is:

```text
summary:
  Tests: 113  Passed: 112  Skipped: 1  Failed: 0 Erred: 0
```

The system and integration groups each run two passing tests. For an uncoloured
report, run `make check-runner-ltest-no-color`. The complete native check below
also runs ordinary EUnit and both custom-listener colour modes. The
[test sources](test/) provide more examples of assertions, generators, named
and nested sets, fixtures and skip reporting.

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

Generate the local reference with `make docs`, which uses the published ldoc
Rebar3 plugin. Open `_build/default/ldoc/current/site/index.html`.

## License

BSD 3-Clause License. See [`LICENSE`](LICENSE).

```text
Copyright © 2013-2026, Duncan McGreggor <oubiwann@gmail.com>

Copyright © 2014, Døkkarr Hirðisson <dokkarr@lfe.io>,
                  Joshua Schairbaum <joshua.schairbaum@gmail.com>

Copyright © 2016, Eric Bailey <eric@ericb.me>,
                  jsc <jonas.skovsgaard.christensen@gmail.com>
```

[hex-downloads]: https://img.shields.io/hexpm/dt/ltest.svg
[hex-package]: https://hex.pm/packages/ltest
[hex-docs-badge]: https://img.shields.io/badge/hex-docs-blue.svg
[hex-docs]: https://hexdocs.pm/ltest/
[ltest-logo]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/ltest-logo-small.png
[ltest-logo-large]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/ltest-logo-large.png
[report-failing]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/screen-2-1.png
[summary-failing]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/screen-2-2.png
[report-passing]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/screen-1-1.png
[summary-passing]: https://raw.githubusercontent.com/lfex/ltest/refs/heads/release/0.14.x/priv/images/screen-1-2.png
