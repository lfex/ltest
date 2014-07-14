#####
lunit
#####

*A Unit, Integration, and System Tests Framework for LFE*


Introduction
============

This project is the successor of `lfeunit`_. That project is now deprecated;
lunit should be used instead.

The original implementation of lunit (as lfeunit) was made due to some
difficulties in parsing the Erlang include file for EUnit, ``eunit.hrl``, by
LFE (it didn't convert all the Erlang macros). That has since been fixed.

Since then, new features have landed in lunit making the creation of not only
unit tests, but system and integration tests, easier and more consistent. These
are briefly outlined in the next section.


Features
--------

* ``(deftest ...)`` for standard unit tests
* ``(deftestgen ...)`` for writing tests with generators, including the
  standard EUnit test fixtures (see naming caveat below)
* ``(deftestskip ...)`` for skipping unit tests
* ``(list ...)``-wrapped tests (of arbitrary depth) for use as test sets
* ``(tuple ...)``-wrapped tests for naming/describing tests (first element
  of tuple)
* ``(behaviour lunit-unit)`` - annotating a test module to be run as a unit
  test
* ``(behaviour lunit-integration)`` - annotating a test module to be run as an
  integration test
* ``(behaviour lunit-system)`` - annotating a test module to be run as a
  system test


Legacy Support
--------------

With version 0.1.0, lunit (lfeunit) changed its API. Functions were converted to macros,
and these were renamed from ``assert-*`` to ``is-*``.

If you have projects that are still using either the previous release (0.0.1) or
old development snapshots and you want to continue using these, you can update
your ``rebar.config`` to point to "old-style" instead of "master", for example:

.. code:: erlang

    {deps, [
        {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "develop"}},
        {lfeunit, ".*", {git, "git://github.com/lfe/lfeunit.git", "old-style"}}
      ]}.


Dogfood
=======

``lunit`` writes its unit tests in ``lunit`` :-) You can run them from the
project directory:

.. code:: bash

    $ make check

Which will give you output similar to the following:

.. code:: text

    ------------------
    Running unit tests ...
    ------------------

    ======================== EUnit ========================
    module 'lunit-basic-tests'
      is ............................................. [ok]
      is-with-one-phrase-deftest ..................... [ok]
      is-with-two-phrase-deftest ..................... [ok]
      is-with-many-phrase-deftest .................... [ok]
      is-fail .............................. [0.003 s] [ok]
      is-not ......................................... [ok]
      is-not-fail .................................... [ok]
      is-equal ....................................... [ok]
      is-equal-fail .................................. [ok]
      is-not-equal ................................... [ok]
      is-not-equal-fail .............................. [ok]
      is-exception ................................... [ok]
      is-exception-wrong-class ....................... [ok]
      is-exception-wrong-term ........................ [ok]
      is-exception-unexpected-success ................ [ok]
      is-error ....................................... [ok]
      is-error-wrong-term ............................ [ok]
      is-error-unexpected-success .................... [ok]
      is-throw ....................................... [ok]
      is-throw-wrong-term ............................ [ok]
      is-throw-unexpected-success .................... [ok]
      is-exit ........................................ [ok]
      is-exit-wrong-term ............................. [ok]
      is-exit-unexpected-success ..................... [ok]
      is-match ....................................... [ok]
      is-match-fail .................................. [ok]
      Total module test time: 0.081 s
    module 'lunit-fixture-tests'
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      foreach-test-case .............................. [ok]
      foreach-test-case .............................. [ok]
      setup-test-case ................................ [ok]
      setup-test-case ................................ [ok]
      foreach-test-case .............................. [ok]
      foreach-test-case .............................. [ok]
      Total module test time: 0.035 s
    module 'lunit-generated-tests'
      one-lambda ..................................... [ok]
      one-lambda-in-list ............................. [ok]
      many-lambdas-in-list ........................... [ok]
      many-lambdas-in-list ........................... [ok]
      many-lambdas-in-list ........................... [ok]
      lambda-with-nested-testset ..................... [ok]
      Total module test time: 0.017 s
    module 'lunit-named-tests'
      named-is ....................................... [ok]
      named-is-not-fail .............................. [ok]
      named-testset-with-one ......................... [ok]
      named-testset-with-two ......................... [ok]
      named-testset-with-three ....................... [ok]
      named-testset-nested ........................... [ok]
      named-testset-deeply-nested .................... [ok]
      Total module test time: 0.021 s
    module 'lunit-testset-tests'
      testset-with-one ............................... [ok]
      testset-with-two ............................... [ok]
      testset-with-three ............................. [ok]
      testset-nested ................................. [ok]
      testset-deeply-nested .......................... [ok]
      Total module test time: 0.015 s
    =======================================================
      All 56 tests passed.


Using lunit
===========


Adding lunit to Your Project
----------------------------

In order to use lunit in your project, all you need to do is add a rebar dep.
In your ``rebar.config`` file, simply add an extra line for ``lunit``:

.. code:: erlang

    {deps, [
        {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "master"}},
        {lunit, ".*", {git, "git://github.com/lfex/lunit.git", "master"}}
      ]}.

Once you write some tests (see below for how to do that), you can then do this:

.. code:: bash

    $ lfetool tests build
    $ lfetool tests unit


Structuring Your Unit Tests
----------------------------

We recommend *not* putting your unit tests directly in your modules, but rather
creating a top-level directory in your project called ``test``. In ``test``,
create a test cases module for every module your project has, e.g.,
``test/myproj-base-tests.lfe`` and ``test/myproj-util-tests.lfe``. Obviously,
if it makes sense to break things up in a more fine-grained manner, feel free
to do so :-)

Furthermore, LFE projects support a standard directory layout for separating
unit, integration, and system tests. These are written as modules in their own
directories, but compiled to the standard ``.eunit`` directory. Modules of a
particular type (e.g., unit, integration, etc.) are distinguished by a module
name prefix.

For a working example of such a structure, see the layout of the ``lunit``
project itself: it uses just such a setup.


Naming Rules
------------

Keep in mind that your tests will be compiled to ``.beam`` and then run with
Erlang's eunit module. As such, your tests need to following the same
conventions that eunit establishes:

* Test module filenames should end in ``-tests``, e.g.,
  ``some-module-tests.lfe``. (In older versions of LFE, it may
  have been required to name test module filenames with ``_tests``, however
  this is no longer the case.)

* Test module and filename need to be the same, minus the extension. For
  example, ``test/unit/unit-my-module-tests.lfe`` needs to be declared as
  ``(defmodule unit-my-module-tests ...) in the test case module``.

* If you chose *not* to use the ``deftest`` macro to build each unit test
  function, you will need to name your unit test functions with ``_test``
  appended to them. For example,
  ``(defun unit-my-function-negagive-check_test () ...)``. We recommend,
  however, that you use ``deftest`` instead, and obviate the need for ``_test
  ()`` boilerplate.

**Naming rules with fixtures**: If you choose to use named functions instead of
``lambda``s for your fixtures or if your ``lambda``s make calls to functions --
all of those need to be standard, unquoted Erlang atoms. In otherwords: no
dashes; you must use underscores.


Creating Unit Tests
-------------------

lunit is entirely macro-based. lunit uses LFE to parse the Erlang macros in
the eunit header file. It also provides its own header file which defines macros
whose main purpose is to wrap the eunit macros in a more Lispy form.

lunit also provides a syntactic sugar macro for defining tests: ``deftest``.
Instead of writing something like this for your unit tests:

.. code:: cl

    (defun unit-my-function-test ()
      ...)

You can use ``deftest`` to write this:

.. code:: cl

    (deftest unit-my-function
      ...)

Note that the ``-test`` is no longer needed, nor is the empty argument list.

If you would like to use EUnit's fixtures feature, you must use another macro:

.. code:: cl

    (deftestgen unit-my-function
      ...)

See above the note on naming functions for use in fixtures.

If you would like tests to be skipped, you can use this macro:

.. code:: cl

    (deftestskip unit-my-function
      ...)

This will simply make the test invisible to EUnit. EUnit doesn't actually
track user-skipped tests; it only tracks tests that are skipped do to issues
as perceived by EUnit.


Here is a more complete example:

.. code:: cl

    (defmodule unit-mymodule-tests
      (behaviour lunit-unit)
      (export all)
      (import
        (from lunit
          (check-failed-assert 2)
          (check-wrong-assert-exception 2))))

    (include-lib "deps/lunit/include/lunit-macros.lfe")

    (deftest is
      (is 'true)
      (is (not 'false))
      (is (not (not 'true))))

    (deftest is-not
      (is-not `'false))

    (deftest is-equal
      (is-equal 2 (+ 1 1)))


lunit is working towards full test coverage; while not there yet, the unit
tests for lunit itself provide the best examples of usage.


Running Your Tests
------------------

The recommended way to run unit tests is to use ``lfetool``. Running
unit tests is now as easy as doing the following:

.. code:: bash

    $ lfetool tests build
    $ lfetool tests unit

Similarly, if your project has defined integration tests, you can do:

.. code:: bash

    $ lfetool tests integration

If you'd like to run unit, integration, and system tests together, run
the following:

.. code:: bash

    $ lfetool tests all


.. Links
.. -----
.. _Makefile: Makefile
.. _Google Groups discussion: https://groups.google.com/d/msg/lisp-flavoured-erlang/eJH2m7XK0dM/WFibzgrqP1AJ
.. _Rebar discussion: http://lists.basho.com/pipermail/rebar_lists.basho.com/2011-January/000471.html
.. _lfeunit: https://github.com/lfe/lfeunit/
