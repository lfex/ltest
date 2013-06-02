lfeunit: eunit for LFE
======================

*Caveat Emptor*: This is a new project with **some** implementation done.
Patches welcome!

Currently, when the Erlang eunit header file (`.hrl`) is `include-lib`ed in
LFE, only a few macros make it over. Robert Virding is looking into this, but
until the fix is ready, it would be a fun exercise to implement a subset of
`eunit`'s functionality for LFE. Thus this project ;-)

Dogfood
-------

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


Making lfeunit a Dep in Your Project
------------------------------------

In your ``rebar.config`` file, simply add an extra line for ``lfeunit``

.. code:: erlang

    {deps, [
        {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "develop"}},
        {lfeunit, ".*", {git, "git://github.com/lfe/lfeunit.git", ""}}
      ]}.

And then do the usual:

.. code:: bash

    $ rebar get-deps
    $ rebar compile


Using lfeunit
-------------
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


Structuring Your Unit Tests
----------------------------

We recommend *not* putting your unit tests directly in your modules, but rather
creating a top-level directory in your project called ``test``. In ``test``,
create a test cases module for every module your project has, e.g.,
``test/myproj_base_tests.lfe`` and ``test/myproj_util_tests.lfe``.

For a working example of such a structure, see the layout of the ``lfeunit``
project itself: it uses just such a setup.


Running Your Tests
------------------

I might add some sort of discovery support, but for now just add a crazy target
in your ``Makefile``:

.. code:: Makefile

    check: TEST_MODS = $(wildcard $(TEST_OUT_DIR)/*.beam)
    check: compile compile-tests
        @clear;
        @for FILE in $(TEST_MODS); do \
        F1="$$(basename $$FILE)"; F2=$${F1%.*}; \
        echo $$F2; done|sed -e :a -e '$$!N; s/\n/,/; ta' | \
        ERL_LIBS=$(ERL_LIBS) \
        xargs -I % erl -W0 -pa $(TEST_OUT_DIR) -noshell \
        -eval "eunit:test([%], [verbose])" \
        -s init stop

For full context, see the `Makefile`_ for this project.

.. Links
.. -----
.. _Makefile: Makefile
.. _Google Groups discussion: https://groups.google.com/d/msg/lisp-flavoured-erlang/eJH2m7XK0dM/WFibzgrqP1AJ
.. _Github LFE ticket: https://github.com/rvirding/lfe/issues/31
