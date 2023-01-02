# ltest

[![Build Status][gh-actions-badge]][gh-actions] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tags][github tags badge]][github tags]

[![][ltest-logo]][ltest-logo-large]

*A Unit, Integration, and System Tests Framework for LFE*


## Contents

* [Introduction](#introduction-)
* [Dependencies](#dependencies-)
* [EUnit Compatibility](#eunit-compatibility-)
* [Features](#features-)
* [Using ltest](#using-ltest-)
  * [Adding ltest to Your Project](#adding-ltest-to-your-project-)
  * [Structuring Your Tests](#structuring-your-tests-)
  * [Naming Rules](#naming-rules-)
  * [Creating Unit Tests](#creating-unit-tests-)
  * [Running Your Tests](#running-your-tests-)
  * [The LFE Test Runner](#the-lfe-test-runner-)
* [Dogfood](#dogfood-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

The macros in this library are, in large part, inspired by Clojure's excellent
unit test framework. In addition to provding such a testing DSL for LFE, this
library also includes a visually pleasing test running with coloured output
and obvious errors/test failures. It also provides the ability to skip tests.

## Dependencies [&#x219F;](#contents)

Version 0.11.0 is the first version to support and work with LFE 2.0.

As of version 0.7.0, this project assumes that you have
[rebar3](https://github.com/rebar/rebar3) installed somwhere in your `$PATH`.
It no longer uses the old version of rebar. If you do not wish to use rebar3,
you may use the most recent rebar2-compatible release of ltest: 0.6.3.

## EUnit Compatibility [&#x219F;](#contents)

The tests created with ltest are compatible with EUnit ane can be run from
either Erlang or LFE, using the standard EUnit listener or the ltest
listener (test runner).

### Features [&#x219F;](#contents)

* `(deftest ...)` for standard unit tests
* `(deftestgen ...)` for writing tests with generators, including the
  standard EUnit test fixtures (see naming caveat below)
* `(deftestskip ...)` for skipping unit tests; note that for a test to show up as skipped in the test runner, it has to be `export`ed in the module
* `(list ...)`-wrapped tests (of arbitrary depth) for use as test sets
* `(tuple ...)`-wrapped tests for naming/describing tests (first element
  of tuple)
* `(behaviour ltest-unit)` - annotating a test module to be run as a unit
  test
* `(behaviour ltest-integration)` - annotating a test module to be run as an
  integration test
* `(behaviour ltest-system)` - annotating a test module to be run as a
  system test
* A custom test runner that over-rides EUnit behaviour and aesthetics


## Using `ltest` [&#x219F;](#contents)


### Adding ltest to Your Project [&#x219F;](#contents)

In order to use ltest in your project, all you need to do is add a rebar dep.
Generally, you only need `ltest` when running tests, so it's best to add it as
a dependency in the `test` profile. You'll also need to tell EUnit where to
take your tests from (`eunit_compile_otps`). In your `rebar.config`:

```erlang
{profiles, [
  {test, [
    {deps, [
      {ltest, {git, "git://github.com/lfex/ltest.git", {tag, "0.12.0"}}}
    ]}
    {src_dirs, ["src", "test"]}
  ]}
]}.
```

Once you write some tests (see below for how to do that), you can then do this:

```bash
$ rebar3 as test lfe ltest
```

### Structuring Your Tests [&#x219F;](#contents)

ltest doesn not support putting your unit tests directly in your modules. If
you do this, things may break or not work properly, even though Erlang's EUnit
does support it.

Instead, you should create a top-level directory in your project called
`test`. In `test`, create a test cases module for every module your project
has, e.g., `test/myproj-base-tests.lfe` and `test/myproj-util-tests.lfe`.
Obviously, if it makes sense to break things up in a more fine-grained manner,
feel free to do so :-)

Furthermore, ltest supports separating unit, integration, and system tests.
This is done using custom OTP behaviours. For each test cases module you have
created in `./test`, be sure to set the behaviour in the `(defmodule ...)`
form. For instance:

```cl
  (defmodule my-unit-tests
    (behaviour ltest-unit)
    (export ...))
```
And two more as well:

```cl
  (defmodule my-integration-tests
    (behaviour ltest-integration)
    (export ...))
```

or

```cl
  (defmodule my-system-tests
    (behaviour ltest-system)
    (export ...))
```

For a working example of such a structure, see the layout of the `ltest`
project itself: it uses just such a setup.

To read more about the distinction between unit, integration, and system
tests, check out the Wikipedia
[article on testing](http://en.wikipedia.org/wiki/Software_testing#Testing_levels).

### Naming Rules [&#x219F;](#contents)

Keep in mind that your tests will be compiled to `.beam` and then run with
Erlang's eunit module. As such, your tests need to following the same
conventions that eunit establishes:

* Test module filenames should end in `-tests`, e.g.,
  `some-module-tests.lfe`.

* Test module and filename need to be the same, minus the extension. For
  example, `test/unit-my-module-tests.lfe` needs to be declared as
  `(defmodule unit-my-module-tests ...) in the test case module`.

* If you chose *not* to use the `deftest` macro to build each unit test
  function, you will need to name your unit test functions with `_test`
  appended to them. For example,
  `(defun unit-my-function-negagive-check_test () ...)`. We recommend,
  however, that you use `deftest` instead, and obviate the need for `_test
  ()` boilerplate.

### Creating Unit Tests [&#x219F;](#contents)

ltest is entirely macro-based. ltest uses LFE to parse the Erlang macros in
the eunit header file. It also provides its own header file which defines macros
whose main purpose is to wrap the eunit macros in a more Lispy form.

ltest also provides a syntactic sugar macro for defining tests: `deftest`.
Instead of writing something like this for your unit tests:

```cl

    (defun unit-my-function-test ()
      ...)
```

You can use `deftest` to write this:

```cl

    (deftest unit-my-function
      ...)
```

Note that the `-test` is no longer needed, nor is the empty argument list.

If you would like to use EUnit's fixtures feature, you must use another macro:

```cl
    (deftestgen unit-my-function
      ...)
```

See the unit tests in the `test` directory for example usage.


If you would like tests to be skipped, you can use this macro:

```cl
    (deftestskip unit-my-function
      ...)
```

This will simply make the test invisible to EUnit. EUnit doesn't actually
track user-skipped tests; it only tracks tests that are skipped do to issues
as perceived by EUnit.

However, ltest's test runner *does* track skipped tests and will report
these in its output.

Here is a more complete example:

```cl
    (defmodule unit-mymodule-tests
      (behaviour ltest-unit)
      (export all))

    (include-lib "ltest/include/ltest-macros.lfe")

    (deftest is
      (is 'true)
      (is (not 'false))
      (is (not (not 'true))))

    (deftest is-not
      (is-not `'false))

    (deftest is-equal
      (is-equal 2 (+ 1 1)))
```

ltest is working towards full test coverage; while not there yet, the unit
tests for ltest itself provide the best examples of usage.


### Running Your Tests [&#x219F;](#contents)

The recommended way to run unit tests is to use the LFE plugin got `rebar3`.
Running tests is now as easy as doing the following:

```bash
    $ rebar3 as test lfe ltest
```

That will run any unit tests you have defined. To run integration or system tests
in your project:

``` bash
    $ rebar3 as test lfe ltest -tintegration
    $ rebar3 as test lfe ltest -tsystem
```

To run all tests for your project:

``` bash
    $ rebar3 as test lfe ltest -tall
```

### The LFE Test Runner [&#x219F;](#contents)

ltest now includes a test runner which overrides the EUnit handlers with its
own. Here's what the output looks like with failing and skipped tests:

<img src="priv/images/screen-2-1.png"/>

End:

<img src="priv/images/screen-2-2.png"/>

And here's what passing tests looks like:

<img src="priv/images/screen-1-1.png"/>

End:

<img src="priv/images/screen-1-2.png"/>

The rest of the `make` targets still use lfetool, and will continue to do
so, since lfetool will be updating to use ltest's new runner. If you'd like
to track the progress on these, here are the related tickets:
 * https://github.com/lfex/ltool/issues/4
 * https://github.com/lfe/lfetool/issues/160


## Dogfood [&#x219F;](#contents)

`ltest` writes its unit tests in `ltest` :-) You can run them from the
project directory:

```bash
    $ make check-runner-ltest
```

Which will give you output similar to the following:

```
================================ ltest =================================

------------------------------ Unit Tests ------------------------------

module: ltest-basic-tests
  is ................................................................ [ok]
  are* .............................................................. [ok]
  is_with_one_phrase_deftest ........................................ [ok]
  is_with_two_phrase_deftest ........................................ [ok]
  is_with_many_phrase_deftest ....................................... [ok]
  is_fail ........................................................... [ok]
  is_not ............................................................ [ok]
  is_not_fail ....................................................... [ok]
  is_equal .......................................................... [ok]
  is_equal_fail ..................................................... [ok]
  is_not_equal ...................................................... [ok]
  is_not_equal_fail ................................................. [ok]
  is_exception ...................................................... [ok]
  is_exception_wrong_class .......................................... [ok]
  is_exception_wrong_term ........................................... [ok]
  is_exception_unexpected_success ................................... [ok]
  is_not_exception .................................................. [ok]
  is_not_exception_exit ............................................. [ok]
  is_not_exception_throw ............................................ [ok]
  is_error .......................................................... [ok]
  is_error_wrong_term ............................................... [ok]
  is_error_unexpected_success ....................................... [ok]
  is_throw .......................................................... [ok]
  is_throw_wrong_term ............................................... [ok]
  is_throw_unexpected_success ....................................... [ok]
  is_exit ........................................................... [ok]
  is_exit_wrong_term ................................................ [ok]
  is_exit_unexpected_success ........................................ [ok]
  is_match .......................................................... [ok]
  is_match_fail ..................................................... [ok]
  time: 94ms

module: ltest-cancelled-tests
  setup_test_case ................................................... [ok]
  Another unused test ............................................... [ok]
  time: 17ms

module: ltest-fixture-tests
  setup_test_case ................................................... [ok]
  setup_test_case ................................................... [ok]
  Named setup test .................................................. [ok]
  setup_test_case ................................................... [ok]
  setup_test_case ................................................... [ok]
  Named setup test .................................................. [ok]
  setup_test_case ................................................... [ok]
  setup_test_case ................................................... [ok]
  Named setup test .................................................. [ok]
  foreach_test_case ................................................. [ok]
  foreach_test_case ................................................. [ok]
  setup_test_case ................................................... [ok]
  setup_test_case ................................................... [ok]
  Named setup test .................................................. [ok]
  foreach_test_case ................................................. [ok]
  foreach_test_case ................................................. [ok]
  time: 49ms

module: ltest-fixturecase-tests
  setup_tc_test_case ................................................ [ok]
  setup_tc_test_case ................................................ [ok]
  Named test in setup-tc ............................................ [ok]
  setup_tc_test_case ................................................ [ok]
  setup_tc_test_case ................................................ [ok]
  Named test in setup-tc ............................................ [ok]
  setup_tc_test_case ................................................ [ok]
  setup_tc_test_case ................................................ [ok]
  Named test in setup-tc ............................................ [ok]
  foreach_tc_test_case .............................................. [ok]
  foreach_tc_test_case .............................................. [ok]
  Named foreach-tc test ............................................. [ok]
  setup_tc_test_case ................................................ [ok]
  setup_tc_test_case ................................................ [ok]
  Named test in setup-tc ............................................ [ok]
  foreach_tc_test_case .............................................. [ok]
  foreach_tc_test_case .............................................. [ok]
  Named foreach-tc test ............................................. [ok]
  time: 56ms

module: ltest-generated-tests
  is* ............................................................... [ok]
  is_not*_in_list ................................................... [ok]
  many_generators_in_list ........................................... [ok]
  many_generators_in_list ........................................... [ok]
  many_generators_in_list ........................................... [ok]
  nested_test_set ................................................... [ok]
  nested_test_set ................................................... [ok]
  nested_test_set ................................................... [ok]
  nested_test_set ................................................... [ok]
  nested_test_set ................................................... [ok]
  nested_test_set ................................................... [ok]
  time: 34ms

module: ltest-named-tests
  named_is .......................................................... [ok]
  named_is_not_fail ................................................. [ok]
  named_testset_with_one ............................................ [ok]
  named_testset_with_two ............................................ [ok]
  named_testset_with_three .......................................... [ok]
  named_testset_nested .............................................. [ok]
  named_testset_deeply_nested ....................................... [ok]
  time: 22ms

module: ltest-skipped-tests
  bogus_test_will_be_skipped ...................................... [skip]
  time: 1ms

module: ltest-testset-tests
  testset_with_one .................................................. [ok]
  testset_with_two .................................................. [ok]
  testset_with_three ................................................ [ok]
  testset_nested .................................................... [ok]
  testset_deeply_nested ............................................. [ok]
  time: 16ms

summary:
  Tests: 90  Passed: 89  Skipped: 1  Failed: 0 Erred: 0
  Total time: 289ms


----------------------------- System Tests -----------------------------

There were no system tests found.


-------------------------- Integration Tests ---------------------------

There were no integration tests found.


========================================================================
```


## License [&#x219F;](#contents)

BSD 3-Clause License

```
Copyright © 2013-2021, Duncan McGreggor <oubiwann@gmail.com>

Copyright © 2014, Døkkarr Hirðisson <dokkarr@lfe.io>,
                  Joshua Schairbaum <joshua.schairbaum@gmail.com>

Copyright © 2016, Eric Bailey <eric@ericb.me>, 
                  jsc <jonas.skovsgaard.christensen@gmail.com>
```





<!-- Named page links below: /-->

[ltest-logo]: priv/images/ltest-logo-small.png
[ltest-logo-large]: priv/images/ltest-logo-large.png
[lr3-logo]: priv/images/logo.png
[org]: https://github.com/lfe-rebar3
[github]: https://github.com/lfex/ltest
[gitlab]: https://gitlab.com/lfex/ltest
[gh-actions-badge]: https://github.com/lfex/ltest/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfex/ltest/actions
[lfe]: https://github.com/lfe/lfe
[lfe badge]: https://img.shields.io/badge/lfe-2.1.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-19%20to%2025-blue.svg
[versions]: https://github.com/lfex/ltest/blob/master/.travis.yml
[github tags]: https://github.com/lfex/ltest/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/ltest.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/ltest.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/ltest
[hex downloads]: https://img.shields.io/hexpm/dt/ltest.svg
