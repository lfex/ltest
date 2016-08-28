(defmodule ltest-cancel-tests
  "This module is meant to test
   the ability to recognize cancelled
   tests (e.g.  fixture tests that
   have bad contents).

   In order to enable this test, take the
   number one (1) out of the behaviour name
   below, and you should see
   \"One or more tests were cancelled.\" in
   the runner output.

   It is disabled because normally you want
   to see the test suite finishing without
   errors.
  "
  (behaviour ltest-unit1))

(include-lib "include/ltest-macros.lfe")

(defun set-up () 'ok)

(defun setup_test_case (set-up-result)
  "This test causes cancellation because of missing comma."
  `[ (is-not-equal* 'this-causes-cancellation 'because-of-comma-missing)
     ,(tuple "Another unused test" (is-not-equal* 'to-be 'or-not-to-be))])

(deftestgen setup-setup
  `#(setup ,(defsetup set-up) ,#'setup_test_case/1))

