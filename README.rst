lfeunit: eunit for LFE
======================

*Caveat Emptor*: This is a new project with very little implementation done yet!

Currently, when the Erlang eunit header file (`.hrl`) is `include-lib`ed in
LFE, only a few macros make it over. Robert Virding is looking into this, but
until the fix is ready, it would be a fun exercise to implement a subset of
`eunit`'s functionality for LFE. Thus this project ;-)


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
We encourage you to use ``lfeunit`` in a way that is similar to ``eunit``: via
library inclusion:

.. code:: cl

    (defmodule mymodule_tests
      (export all))

    ; Define a macro/constant to make up for LFE's lack of ?LINE support.
    (defmacro LINE () `'unknown)

    (include-lib "include/lfeunit.lfe")

    (defun assert_test ()
      (assert `'true)
      (assert '(not 'false))
      (assert '(not (not 'true))))

However, you also have the option of using ``lfeunit`` like any other LFE or
Erlang library:

.. code:: cl

    (defmodule mymodule_tests
      (export all)
      (import (from lfeunit (assert 1) (assert-not 1) (assert-equal 2))))

    (defun assert_test ()
      (assert `'true)
      (assert '(not 'false))
      (assert '(not (not 'true))))

However, when you do it this way, the data in the failed results will not
accurately reflect your current module (e.g., ``(MODULE)`` will return the
``lfeunit`` library module, not your module).


Running Your Tests
------------------

I might add some sort of discovery support, but for now just add a crazy target
in your ``Makefile``:

.. code:: Makefile

    check: TEST_MODS = $(wildcard $(TEST_OUT_DIR)/*.beam)
    check: compile compile-tests
        @#rebar eunit verbose=1 skip_deps=true
        @for FILE in $(wildcard $(TEST_OUT_DIR)/*.beam); do \
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