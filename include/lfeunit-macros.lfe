(include-lib "eunit/include/eunit.hrl")

(defmacro deftestsuite arg
  "This macro provides some syntactic sugar and wrapping for defining test
  modules."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defmodule ,(list_to_atom (++ (atom_to_list name) '"-tests"))
       (export all)
       (import
         (from lfeunit-util
           (check-failed-assert 2)
           (check-wrong-assert-exception 2)))
       ,@body)))

(defmacro deftest arg
  "This macro is inspired by the Clojure unit test macro."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
       ,@body)))

(defmacro is (bool-expression)
  "This macro takes an expression that returns a boolean value. If the
  expression does not evaluate as a truth value, an error is returned."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "This macro takes an expression that returns a boolean value. If the
  expression does not evaluate as false value, an error is returned."
  `(assertNot ,bool-expression))

(defmacro is-equal (value expression)
  "This macro checks the equality between a passed value and a quoated
  expression."
  `(assertEqual ,value ,expression))

(defmacro is-not-equal (value expression)
  "This macro checks the inequality between an expected value and a passed
  expression."
  `(assertNotEqual ,value ,expression))

(defmacro is-exception (expected-class expected-term expression)
  "This macro check that the passeed expression raises the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(assertException ,expected-class ,expected-term ,expression))

(defmacro is-not-exception (expected-class expected-term expression)
  "This macro check that the passeed expression does not raise the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(not (is-exception ,expected-class ,expected-term ,expression)))

(defmacro is-error (error body)
  "This macro is a convenience macro for is-exception with an
  exception class of 'error."
  `(assertError ,error ,body))

(defmacro is-not-error (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'error."
  `(is-not-exception 'error ,expected-term ,expression))

(defmacro is-exit (expected-term expression)
  "This macro is a convenience macro for is-exception with an
  exception class of 'exit."
  `(assertExit ,expected-term ,expression))

(defmacro is-not-exit (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'exit."
  `(is-not-exception 'exit ,expected-term ,expression))

(defmacro is-throw (expected-term expression)
  "This macro is a convenience macro for is-exception with an
  exception class of 'throw."
  `(assertThrow ,expected-term ,expression))

(defmacro is-not-throw (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'throw."
  `(is-not-exception 'throw ,expected-term ,expression))

(defmacro is-match (guard expression)
  ""
  `(assertMatch ,guard ,expression))
