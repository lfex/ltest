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

    ------------------
    Running unit tests ...
    ------------------

    ======================== EUnit ========================
    module 'unit-lfeunit-fixture-tests'
      setup-setup .................................... [ok]
      setup-setup-cleanup ............................ [ok]
      foreach-setup .................................. [ok]
      foreach-setup-cleanup .......................... [ok]
      Total module test time: 0.003 s
    module 'unit-lfeunit-tests'
      is ............................................. [ok]
      is-with-one-phrase-deftest ..................... [ok]
      is-with-two-phrase-deftest ..................... [ok]
      is-with-many-phrase-deftest .................... [ok]
      is-fail .............................. [0.007 s] [ok]
      is-not ......................................... [ok]
      is-not-fail .......................... [0.007 s] [ok]
      is-equal ....................................... [ok]
      is-equal-fail ........................ [0.007 s] [ok]
      is-not-equal ................................... [ok]
      is-not-equal-fail .................... [0.007 s] [ok]
      is-exception ................................... [ok]
      is-exception-wrong-class ............. [0.007 s] [ok]
      is-exception-wrong-term .............. [0.007 s] [ok]
      is-exception-unexpected-success ...... [0.007 s] [ok]
      is-error ....................................... [ok]
      is-error-wrong-term .................. [0.007 s] [ok]
      is-error-unexpected-success .......... [0.007 s] [ok]
      is-throw ....................................... [ok]
      is-throw-wrong-term .................. [0.007 s] [ok]
      is-throw-unexpected-success .......... [0.007 s] [ok]
      is-exit ........................................ [ok]
      is-exit-wrong-term ................... [0.007 s] [ok]
      is-exit-unexpected-success ........... [0.007 s] [ok]
      is-match ....................................... [ok]
      is-match-fail ........................ [0.007 s] [ok]
      Total module test time: 0.011 s
    =======================================================
      All 30 tests passed.


Using lfeunit
=============


Adding lfeunit to Your Project
------------------------------

In order to use lfeunit in your project, all you need to do is add a Rebar dep.
In your ``rebar.config`` file, simply add an extra line for ``lfeunit``:

.. code:: erlang

    {deps, [
        {lfe, ".*", {git, "git://github.com/rvirding/lfe.git", "develop"}},
        {lfeunit, ".*", {git, "git://github.com/lfe/lfeunit.git", "master"}}
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

Furthermore, LFE projects support a standard directory layout for separating
unit, integration, and system tests. These are written as modules in their own
directories, but compiled to the standard ``.eunit`` directory. Modules of a
particular type (e.g., unit, integration, etc.) are distinguished by a module
name prefix.

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
  example, ``test/unit/unit-my-module-tests.lfe`` needs to be declared as
  ``(defmodule unit-my-module-tests ...) in the test case module``.

* If you chose *not* to use the ``deftest`` macro to build each unit test
  function, you will need to name your unit test functions with ``_test``
  appended to them. For example,
  ``(defun unit-my-function-negagive-check_test () ...)``. We recommend,
  however, that you use ``deftest`` instead, and obviate the need for ``_test
  ()`` boilerplate.


Creating Unit Tests
-------------------

lfeunit is entirely macro-based. lfeunit uses LFE to parse the Erlang macros in
the eunit header file. It also provides its own header file which defines macros
whose purpose is to wrap the eunit macros in a more Lispy form.

lfeunit also provides a syntactic sugar macro for defining tests: ``deftest``.
Instead of writing something like this for your unit tests:

.. code:: cl

    (defun unit-my-function-test ()
      ...)

You can use ``deftest`` to write this:

.. code:: cl

    (deftest unit-my-function
      ...)

Note that the ``-test`` is no longer needed, nor is the empty argument list.

Here is a more complete example:

.. code:: cl

    (defmodule unit-mymodule-tests
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
little more work. Prior to ``lfetool`` each project had to include make
targets for compiling unit tests. ``lfetool`` now does this for you. Running
tests is now as easy as doing the following:

.. code:: bash

    $ lfetool tests build
    $ lfetool tests unit

or

.. code:: bash

    $ lfetool tests all

If you would like to see how to do this manually, you should examine the source
code of ``lfetool``. In particular, the file
``plugins/lfetool/templates/lfetool.tmpl`` in the lfetool source code.

Also, for an example of testing targets that are using ``lfetool``, see the
`common.mk`_ file for this project.

Once your project is using these targets, you can simply
execute the any one of the following to run your tests:

.. code:: bash

    $ make check
    $ make check-unit-only
    $ make check-integration-only
    $ make check-system-only
    $ make check-unit-with-deps
    $ make check-unit
    $ make check-integration
    $ make check-system
    $ make check-all-with-deps
    $ make check-all

The make targets suffixed with ``-only`` assume that your unit tests have
already been compiled (as such, these run very quickly). The other targets do
various levels of compiling (deps, tests, etc.) for you, at which point your
``.lfe`` test files will be compiled to ``.beam`` and placed in the testing
directory (``.eunit``). This is the directory that all ``check*`` targets
use to look for the tests to run.


.. Links
.. -----
.. _Makefile: Makefile
.. _Google Groups discussion: https://groups.google.com/d/msg/lisp-flavoured-erlang/eJH2m7XK0dM/WFibzgrqP1AJ
.. _Github LFE ticket: https://github.com/rvirding/lfe/issues/31
.. _Rebar discussion: http://lists.basho.com/pipermail/rebar_lists.basho.com/2011-January/000471.html
