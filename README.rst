######################
lfeunit: eunit for LFE
######################

*Caveat Emptor*: This is a new project with **some** implementation done.
Patches welcome!

Currently, when the Erlang eunit header file (``.hrl``) is ``include-lib`` ed in
LFE, only a few macros make it over. Robert Virding is looking into this, but
until the fix is ready, it would be a fun exercise to implement a subset of
``eunit`` 's functionality for LFE. Thus this project ;-)

Dogfood
=======

``lfeunit`` writes its unit tests in ``lfeunit`` :-) You can run them from the
project directory:

.. code:: bash

    $ make check

Which will give you output similar to the following:

.. code:: text

    ======================== EUnit ========================
    module 'lfeunit_tests'
      lfeunit_tests: assert_test...[0.015 s] ok
      lfeunit_tests: assert-fail_test...ok
      lfeunit_tests: assert-not_test...ok
      lfeunit_tests: assert-not-fail_test...ok
      lfeunit_tests: assert-equal_test...ok
      lfeunit_tests: assert-equal-fail_test...ok
      lfeunit_tests: assert-not-equal_test...ok
      lfeunit_tests: assert-not-equal-fail_test...ok
      lfeunit_tests: assert-exception_test...ok
      lfeunit_tests: assert-exception-wrong-class_test...ok
      lfeunit_tests: assert-exception-wrong-term_test...ok
      lfeunit_tests: assert-exception-unexpected-success_test...ok
      lfeunit_tests: assert-error_test...ok
      lfeunit_tests: assert-error-wrong-term_test...ok
      lfeunit_tests: assert-error-unexpected-success_test...[0.001 s] ok
      lfeunit_tests: assert-throw_test...ok
      lfeunit_tests: assert-throw-wrong-term_test...ok
      lfeunit_tests: assert-throw-unexpected-success_test...ok
      lfeunit_tests: assert-exit_test...ok
      lfeunit_tests: assert-exit-wrong-term_test...ok
      lfeunit_tests: assert-exit-unexpected-success_test...ok
      [done in 0.078 s]
    =======================================================
      All 21 tests passed.


Using lfeunit
=============

Adding lfeunit to Your Project
------------------------------

In order to use lfeunit in your project, all you need to do is add a Rebar dep.
In your ``rebar.config`` file, simply add an extra line for ``lfeunit``:

.. code:: erlang

    {deps, [
        {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "develop"}},
        {lfeunit, ".*", {git, "git://github.com/lfe/lfeunit.git", ""}}
      ]}.

And then do the usual:

.. code:: bash

    $ rebar get-deps
    $ rebar compile


Structuring Your Unit Tests
----------------------------

We recommend *not* putting your unit tests directly in your modules, but rather
creating a top-level directory in your project called ``test``. In ``test``,
create a test cases module for every module your project has, e.g.,
``test/myproj_base_tests.lfe`` and ``test/myproj_util_tests.lfe``.

For a working example of such a structure, see the layout of the ``lfeunit``
project itself: it uses just such a setup.


Naming Rules
------------

Keep in mind that your tests will be compiled to ``.beam`` and then run with
Erlang's eunit module. As such, your tests need to following the same
conventions that eunit establishes:

* Test module names need to end in ``_tests``.

* Test module and filename need to be the same, minus the extension. For
  example, ``test/my-module_tests.lfe`` needs to be declared as
  ``(defmodule my-module_tests ...)``.

* Unit tests need to be named with ``_test`` appended to them. For example,
  ``(defun my-function-negagive-check_test () ...)``.


Creating Unit Tests
-------------------

Due to some current issues in LFE (supporting flexible include paths; see
the `Google Groups discussion`_ and the `Github LFE ticket`_ for more info),
lfeunit is only usable via module import (no include support, a la eunit).

As such, you use lfeunit like any other LFE or Erlang library:

.. code:: cl

    (defmodule mymodule_tests
      (export all)
      (import
        (from lfeunit
          (assert 1)
          (assert-not 1)
          (assert-equal 2))))

    (defun assert_test ()
      (assert `'true)
      (assert '(not 'false))
      (assert '(not (not 'true))))

    (defun assert-not_test ()
      (assert-not `'false'))

    (defun assert-equal_test ()
      (assert-equal 2 '(+ 1 1)))


Running Your Tests
------------------

Rebar doesn't seem to compile lfe unit tests right now (See the
`Rebar discussion`_ for more information about this). As such, we have to do a
little more work. I like to put this work in a Makefile:

.. code:: Makefile

    TEST_DIR = ./test
    TEST_EBIN_DIR = ./.eunit

    compile-tests:
        ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_EBIN_DIR) $(TEST_DIR)/*_tests.lfe

    check: compile-tests
        @clear;
        rebar eunit skip_deps=true verbose=1

For full context and a more robust example, see the `Makefile`_ for this
project.

Once this is updated for your project and in your ``Makefile``, you can simply
execute the following to run your tests:

.. code:: bash

    $ make check

At which point your ``.lfe`` test files will be compiled to ``.beam`` and placed
in a directory where Rebar expects them (``.eunit``). Rebar will then run your
unit tests.

.. Links
.. -----
.. _Makefile: Makefile
.. _Google Groups discussion: https://groups.google.com/d/msg/lisp-flavoured-erlang/eJH2m7XK0dM/WFibzgrqP1AJ
.. _Github LFE ticket: https://github.com/rvirding/lfe/issues/31
.. _Rebar discussion: http://lists.basho.com/pipermail/rebar_lists.basho.com/2011-January/000471.html
