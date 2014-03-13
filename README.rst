######################
lfeunit: eunit for LFE
######################


Introduction
============

*Caveat Emptor*: This is a new project with **some** implementation done.
Patches welcome!

The original implementation of lfeunit was made due to some difficulties in
parsing the ``eunit.hrl`` file fully by LFE (it didn't convert all the Erlang
macros). Robert has since made some enhancements to the LFE Erlang macro
processing code, and it now pulls in everything.

Is there still a need for lfeunit?

Well, perhaps not *need*, but certainly a benefit :-) lfeunit is intended to be
more Lisp-y than simply calling macros from eunit. Futhermore, we hope to
define some macros that will make testing a pleasure in LFE.


Legacy Support
--------------

With version 0.1.0, lfeunit changed its API. Functions were converted to macros,
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

``lfeunit`` writes its unit tests in ``lfeunit`` :-) You can run them from the
project directory:

.. code:: bash

    $ make check

Which will give you output similar to the following:

.. code:: text

    ==> lfeunit (eunit)
    ======================== EUnit ========================
    module 'lfeunit_tests'
      lfeunit_tests: is_test...ok
      lfeunit_tests: is-with-one-phrase-deftest_test...ok
      lfeunit_tests: is-with-two-phrase-deftest_test...ok
      lfeunit_tests: is-with-many-phrase-deftest_test...ok
      lfeunit_tests: is-fail_test...[0.044 s] ok
      lfeunit_tests: is-not_test...ok
      lfeunit_tests: is-not-fail_test...ok
      lfeunit_tests: is-equal_test...ok
      lfeunit_tests: is-equal-fail_test...ok
      lfeunit_tests: is-not-equal_test...ok
      lfeunit_tests: is-not-equal-fail_test...ok
      lfeunit_tests: is-exception_test...ok
      lfeunit_tests: is-exception-wrong-class_test...ok
      lfeunit_tests: is-exception-wrong-term_test...ok
      lfeunit_tests: is-exception-unexpected-success_test...ok
      lfeunit_tests: is-error_test...ok
      lfeunit_tests: is-error-wrong-term_test...ok
      lfeunit_tests: is-error-unexpected-success_test...ok
      lfeunit_tests: is-throw_test...ok
      lfeunit_tests: is-throw-wrong-term_test...ok
      lfeunit_tests: is-throw-unexpected-success_test...ok
      lfeunit_tests: is-exit_test...ok
      lfeunit_tests: is-exit-wrong-term_test...ok
      lfeunit_tests: is-exit-unexpected-success_test...ok
      [done in 0.115 s]
    module 'lfeunit-fixture_tests'
      lfeunit-fixture_tests: setup-setup_test...ok
      lfeunit-fixture_tests: setup-setup-cleanup_test...ok
      lfeunit-fixture_tests: foreach-setup_test...ok
      lfeunit-fixture_tests: foreach-setup-cleanup_test...ok
      [done in 0.011 s]
    =======================================================
      All 28 tests passed.


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
``test/myproj-base-tests.lfe`` and ``test/myproj-util-tests.lfe``. Obviously,
if it makes sense to break things up in a more fine-grained manner, feel free
to do so :-)

For a working example of such a structure, see the layout of the ``lfeunit``
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
  example, ``test/my-module-tests.lfe`` needs to be declared as
  ``(defmodule my-module-tests ...) in the test case module``.

* If you chose *not* to use the ``deftest`` macro to build each unit test
  function, you will need to name your unit test functions with ``_test``
  appended to them. For example,
  ``(defun my-function-negagive-check_test () ...)``. We recommend, however,
  that you use ``deftest`` instead, and obviate the need for ``_test ()``
  boilerplate.

Creating Unit Tests
-------------------

lfeunit is entirely macro-based. lfeunit uses LFE to parse the Erlang macros in
the eunit header file. It also provides its own header file which defines macros
whose purpose is to wrap the eunit macros in a more Lispy form.

lfeunit also provides a syntactic sugar macro for defining tests: ``deftest``.
Instead of writing something like this for your unit tests:

.. code:: cl

    (defun my-function-test ()
      ...)

You can use ``deftest`` to write this:

.. code:: cl

    (deftest my-function
      ...)

Note that the ``-test`` is no longer needed, nor is the empty argument list.

Here is a more complete example:

.. code:: cl

    (defmodule mymodule-tests
      (export all)
      (import
        (from lfeunit-util
          (check-failed-assert 2)
          (check-wrong-assert-exception 2))))

    (include-lib "deps/lfeunit/include/lfeunit-macros.lfe")


    (deftest is
      (is 'true)
      (is (not 'false))
      (is (not (not 'true))))

    (deftest is-not
      (is-not `'false))

    (deftest is-equal
      (is-equal 2 (+ 1 1)))

lfeunit is working towards full test coverage; while not there yet, the unit
tests for lfeunit itself provide the best examples of usage.


Running Your Tests
------------------

Rebar doesn't seem to compile lfe unit tests right now (See the
`Rebar discussion`_ for more information about this). As such, we have to do a
little more work. I like to put this work in a Makefile:

.. code:: Makefile

    TEST_DIR = ./test
    TEST_EBIN_DIR = ./.eunit

    compile-tests:
        ERL_LIBS=$(ERL_LIBS) $(LFEC) -o $(TEST_EBIN_DIR) $(TEST_DIR)/*[_-]tests.lfe

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
