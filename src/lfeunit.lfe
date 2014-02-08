;;;; User lfeunit in your modules in the following manner:
;;;;    (defmodule mymodule_tests
;;;;      export all)
;;;;      (import (from lfeunit (assert-equal 2))))
;;;;
(defmodule lfeunit
  (export all))

(include-lib "eunit/include/eunit.hrl")


(defun is (bool-expression)
  "
  This function takes an expression that returns a boolean value. If the
  expression does not evaluate as a truth value, an error is returned.
  "
  (assert bool-expression))

(defun is-not (bool-expression)
  "
  This function takes an expression that returns a boolean value. If the
  expression does not evaluate as false value, an error is returned.
  "
  (assertNot bool-expression))

(defun is-equal (value expression)
  "
  This function checks the equality between a passed value and a quoated
  expression.
  "
  (assertEqual value expression))

(defun is-not-equal (value expression)
  "
  This function checks the inequality between an expected value and a passed
  expression.
  "
  (assertNotEqual value expression))

(defun is-exception (expected-class expected-term expression)
  "
  This function check that the passeed expression raises the expected exception
  class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef, 'badarith, etc.).
  "
  (assertException expected-class expected-term expression))

(defun is-not-exception (expected-class expected-term expression)
  "
  This function check that the passeed expression does not raise the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.).
  "
  (not (is-exception expected-class expected-term expression)))

(defun is-error (expected-term expression)
  "
  This function is a convenience function for is-exception with an
  exception class of 'error.
  "
  (is-exception 'error expected-term expression))

(defun is-not-error (expected-term expression)
  "
  This function is a convenience function for is-not-exception with an
  exception class of 'error.
  "
  (is-not-exception 'error expected-term expression))

(defun is-exit (expected-term expression)
  "
  This function is a convenience function for is-exception with an
  exception class of 'exit.
  "
  (is-exception 'exit expected-term expression))

(defun is-not-exit (expected-term expression)
  "
  This function is a convenience function for is-not-exception with an
  exception class of 'exit.
  "
  (is-not-exception 'exit expected-term expression))

(defun is-throw (expected-term expression)
  "
  This function is a convenience function for is-exception with an
  exception class of 'throw.
  "
  (is-exception 'throw expected-term expression))

(defun is-not-throw (expected-term expression)
  "
  This function is a convenience function for is-not-exception with an
  exception class of 'throw.
  "
  (is-not-exception 'throw expected-term expression))

(defun is-match (guard expression)
  ""
  (assertMatch guard expression))

(defun is-not-match (guard expression)
  ""
  (not (is-match guard expression)))
