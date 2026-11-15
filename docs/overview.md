# ltest API overview

`ltest` 0.14.0 discovers LFE test modules by marker behavior and executes the
selected compiled modules through EUnit.

## Quick start

Put tests in `test/` and mark each module with the group it belongs to. Include
the macro header to use LFE test definitions and assertions:

```lisp
(defmodule calculator-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest adds-two-numbers
  (is-equal 5 (+ 2 3)))
```

`deftest` defines `adds_two_numbers_test/0`, which EUnit executes. LFE dashes
in generated function names become underscores. Use `ltest-system` or
`ltest-integration` for the other test groups. These marker behaviors declare
no callbacks; they tag modules for discovery. `behaviour_info(callbacks)`
returns an empty list, and other behavior queries return `undefined`.

A marker's `get-modules/0` export returns the placeholder `ok`; it does not
discover test modules. Discovery reads behavior attributes from compiled BEAM
files in `.eunit/` and `_build/*/lib/*/ebin/` beneath the selected project
path. EUnit then selects test functions by its own naming and arity rules.

## Running tests

The Rebar3 LFE provider invokes `ltest:run/1` with its configured test type
and report options. The `rebar3_lfe` 0.5.8 provider exposes `--suite` and
function-oriented `--test`, but has no module `-m` option. These selectors
are available through the public library API; command-line module selection
is not included. The public group functions can also be called directly:

```lisp
(ltest:unit)
(ltest:system #m(color false))
(ltest:integration)
(ltest:all #m(color false))
```

`ltest:default-opts/0` enables color, suite headers, and verbose output; it
selects `ltest-listener` and the `unit` test type. Public group `/1`
functions merge a partial map over these defaults and then force their own
`test-type`; group `/0` functions use the full defaults. Both forms
return EUnit's canonical result. `all/0,1` runs unit, system, and integration
in order even if a group fails, then returns the first non-`ok` result.

`run/0` selects the default unit group. `run/1` merges a partial map with the
defaults and dispatches the selected `test-type`; it maps EUnit `error` to
`{error, tests_failed}`, preserves other results, and returns an explanatory
`{error, Message}` for an unknown type. The EUnit option helpers expect the
keys they read; they do not independently merge defaults.

Pass `modules` to select discovered test modules. `module` and `suite` are
compatibility aliases for the same set. Names match module identities
declared in eligible discovered BEAMs, not filenames, functions, regular
expressions, or wildcards. Values may be one atom, UTF-8 binary, or Unicode
character list, or a nonempty proper list of those scalar forms. A scalar may
contain comma-separated names; ASCII space, tab, carriage return, and newline
are trimmed from token edges. Names are case-sensitive, deduplicated, and
sorted for alias comparison, while execution keeps discovery order. `all` is
an ordinary module name. No selector keeps the existing run-all behavior;
explicit empty values and empty tokens are invalid.

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

Several supplied aliases must have exactly equal normalized sets. Invalid
values return `{error, {invalid_selector, Key, RawValue}}`; disagreement
returns `{error, {conflicting_selectors, Aliases}}`; names absent from the
requested group's discovered BEAMs return
`{error, {unmatched_modules, MissingNames}}`. The runner compares names with
BEAM metadata and does not convert caller text to atoms. It validates the
whole request before EUnit starts, so a partly matching request runs no tests.
`all` validates against the union of unit, system, and integration modules
before starting any group. Empty selected groups succeed. Direct `unit/1`,
`system/1`, and `integration/1` calls force their own group even if
`test-type` disagrees.

This library API does not select individual test functions. `test` may be
absent or `undefined`; any other value returns
`{error, {unsupported_test_selector, RawValue}}` before EUnit runs. `all/0,1`
runs unit, system, and integration in that order even after a selected group
fails, then returns the first non-`ok` result. `run/1` maps only bare EUnit
`error` to `{error, tests_failed}` and preserves tagged selector errors.

The listener implements EUnit's `eunit_listener` callbacks
`init/1`, `handle_begin/3`, `handle_end/3`, `handle_cancel/3`, and
`terminate/2`. Its event status values and lifecycle come from EUnit. An empty
or partial listener option list defaults to color enabled and test type `unit`;
explicit values are retained. Setting `color` to `false` disables ANSI color
sequences. The listener's private `state` record stores counters, elapsed time,
and per-module skip-accounting state; it is not a public data type.

## Test definitions and fixtures

`deftest NAME BODY...` defines a nullary EUnit test. `deftestgen` defines an
EUnit generator ending in `_test_`. `deftestskip` defines a nullary `_skip` function but does not export it.
Export it explicitly (or use `export all`) for ltest to list it as skipped;
ltest does not call its body. The helper classifier accepts only nullary
exported names whose final characters are `_skip`;
`ltest-util:skipped?/1` exposes that predicate.

`deftestcase` defines a fixture case function for use with `deftestcases`.
Printable named tuples in the case body produce named EUnit objects; other
forms become zero-argument test functions. Define each case before passing its
name to `deftestcases`. `defsetup` and `defteardown` create fixture functions
that call the named setup `/0` and teardown `/1` functions. These macros keep
EUnit fixture conventions; see EUnit's documentation for setup and teardown
semantics.

## Assertion and test-object reference

The header contains 42 public top-level macros. The first two sections provide
OTP compatibility names and test/fixture definitions. Assertion macros run
immediately. Test-object macros ending in `*` construct EUnit objects that
EUnit evaluates later, except `are*`, which substitutes complete rows for a
nonempty variable list and runs the resulting assertions immediately.

| Macro | Call form | Behavior |
| --- | --- | --- |
| `otp>=18?` | `(otp>=18?)` | Return whether the running OTP release is at least 18; return false if it cannot be parsed. |
| `assertion-failed` | `(assertion-failed)` | Select the assertion failure tag used by the OTP version. |
| `assert-equal-failed` | `(assert-equal-failed)` | Select the equality assertion failure tag used by the OTP version. |
| `assert-not-equal-failed` | `(assert-not-equal-failed)` | Select the inequality assertion failure tag used by the OTP version. |
| `assert-exception-failed` | `(assert-exception-failed)` | Select the exception assertion failure tag used by the OTP version. |
| `assert-match-failed` | `(assert-match-failed)` | Select the pattern-match assertion failure tag used by the OTP version. |
| `deftest` | `(deftest name body...)` | Define a nullary `name_test/0` from the body forms. |
| `deftestgen` | `(deftestgen name body...)` | Define a nullary `name_test_/0` generator. |
| `deftestskip` | `(deftestskip name body...)` | Define a nullary `name_skip/0`; export it for ltest to list it without running. |
| `defsetup` | `(defsetup function-name)` | Return a zero-argument fixture setup function calling `function-name/0`. |
| `defteardown` | `(defteardown function-name)` | Return a one-argument fixture teardown function calling `function-name/1`. |
| `deftestcase` | `(deftestcase function-name (args...) form...)` | Define a named fixture case; printable named tuples become named tests. |
| `deftestcases` | `(deftestcases function-name...)` | Build a fixture generator from previously defined `deftestcase` functions. |
| `is` | `(is expression)` | Assert that the expression evaluates to `true`. |
| `is-not` | `(is-not expression)` | Assert that the expression evaluates to `false`. |
| `is-match` | `(is-match pattern expression)` | Assert that the expression matches the pattern, with an EUnit diagnostic. |
| `is-not-match` | `(is-not-match pattern expression)` | Assert that the expression does not match the pattern. |
| `is-equal` | `(is-equal expected expression)` | Assert exact term equality (`=:=`); `1` and `1.0` differ. |
| `is-not-equal` | `(is-not-equal expected expression)` | Assert inequality under exact term equality (`=:=`); `1` and `1.0` differ. |
| `is-exception` | `(is-exception expression)` or `(is-exception class term expression)` | Assert an exception; the three-argument form checks class and term. |
| `is-not-exception` | `(is-not-exception expression)` or `(is-not-exception class term expression)` | One argument asserts no exception; three arguments reject only the selected class and term. |
| `is-error` | `(is-error expression)` or `(is-error term expression)` | Assert an error, optionally matching its term. |
| `is-not-error` | `(is-not-error expression)` or `(is-not-error term expression)` | Reject the selected error term; other exception classes may pass. |
| `is-exit` | `(is-exit expression)` or `(is-exit term expression)` | Assert an exit, optionally matching its term. |
| `is-not-exit` | `(is-not-exit expression)` or `(is-not-exit term expression)` | Reject the selected exit term; other exception classes may pass. |
| `is-throw` | `(is-throw expression)` or `(is-throw term expression)` | Assert a throw, optionally matching its term. |
| `is-not-throw` | `(is-not-throw expression)` or `(is-not-throw term expression)` | Reject the selected throw term; other exception classes may pass. |
| `is*` | `(is* expression)` | Construct a lazy EUnit object for `is`. |
| `are*` | `(are* (variables...) expression values...)` | With a nonempty variable list and complete value rows, substitute each row into immediate assertions. |
| `is-not*` | `(is-not* expression)` | Construct a lazy EUnit object for `is-not`. |
| `is-match*` | `(is-match* pattern expression)` | Construct a lazy EUnit object for `is-match`. |
| `is-not-match*` | `(is-not-match* pattern expression)` | Construct a lazy EUnit object for `is-not-match`. |
| `is-equal*` | `(is-equal* expected expression)` | Construct a lazy EUnit object checking exact term equality (`=:=`); `1` and `1.0` differ. |
| `is-not-equal*` | `(is-not-equal* expected expression)` | Construct a lazy EUnit object checking inequality under exact term equality (`=:=`). |
| `is-exception*` | `(is-exception* expression)` or `(is-exception* class term expression)` | Construct a lazy object checking an exception, optionally by class and term. |
| `is-not-exception*` | `(is-not-exception* expression)` or `(is-not-exception* class term expression)` | One argument asserts no exception; three arguments reject only the selected class and term. |
| `is-error*` | `(is-error* expression)` or `(is-error* term expression)` | Construct a lazy object checking an error, optionally by term. |
| `is-not-error*` | `(is-not-error* expression)` or `(is-not-error* term expression)` | Reject the selected error term; other exception classes may pass. |
| `is-exit*` | `(is-exit* expression)` or `(is-exit* term expression)` | Construct a lazy object checking an exit, optionally by term. |
| `is-not-exit*` | `(is-not-exit* expression)` or `(is-not-exit* term expression)` | Reject the selected exit term; other exception classes may pass. |
| `is-throw*` | `(is-throw* expression)` or `(is-throw* term expression)` | Construct a lazy object checking a throw, optionally by term. |
| `is-not-throw*` | `(is-not-throw* expression)` or `(is-not-throw* term expression)` | Reject the selected throw term; other exception classes may pass. |

The one-argument `is-not-exception` form uses wildcard class and term patterns,
so any exception violates it. With explicit class and term, only that pair is
rejected and a different exception may pass. The class-specific negative
macros `is-not-error`, `is-not-exit`, and `is-not-throw` reject only their
selected exception class and term; other classes may pass.

`are*` takes a nonempty list of variable names, an expression, then values
grouped into complete rows matching that list. It substitutes each row into
the expression and runs the resulting assertions immediately rather than
building delayed EUnit test objects. Substitution walks lists; it does not
recursively rewrite arbitrary tuples or maps. Use `is*` and the other delayed
forms when an EUnit test object is needed.

## Skip helpers and lower-level API

`ltest:get-skip-funcs/1` returns exported `{name, 0}` skip functions, while
`ltest:get-skipped-tests/1` returns character-list names without the `_skip`
suffix.
`check-skip-funcs/1` and `check-skipped-tests/1` are lower-level list
transforms that retain one result-or-`false` slot per input export.
`ltest-util:filter-skipped/1` filters exports using the same classifier.
`skip-test-patt/0` and `skip-test-group-patt/0` are retained regular-expression
compatibility values; they are not the classifier implementation.

Every function is exported by the current source modules. The generated
reference therefore includes both the runner API and implementation helpers.
Some helpers require a readable BEAM or a module resolvable on the current code
path; their BEAM and file-shape errors propagate. Header-only macros are
documented here because LFE does not embed them as native function entries.
