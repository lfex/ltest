(defmodule lfeunit_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "include/lfeunit-macros.lfe")


(defun is_test ()
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))

(defun is-fail_test ()
  (try
    (progn
      (is 'false)
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value 'assertion_failed)))))

(defun is-not_test ()
  (is-not 'false)
  (is-not (not 'true))
  (is-not (not (not 'false))))

(defun is-not-fail_test ()
  (try
    (progn
      (is-not 'true)
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value 'assertion_failed)))))

(defun is-equal_test ()
  (is-equal 1 1)
  (is-equal 1 (+ 1 0))
  (is-equal 1 (- 2 1)))

(defun is-equal-fail_test ()
  (try
    (progn
      (is-equal 1 2)
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value 'assertEqual_failed)))))

(defun is-not-equal_test ()
  (is-not-equal 0 1)
  (is-not-equal 0 (+ 1 0))
  (is-not-equal 0 (- 2 1)))

(defun is-not-equal-fail_test ()
  (try
    (progn
      (is-not-equal 1 (+ 1 0))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value 'assertNotEqual_failed)))))

(defun is-exception_test ()
  (is-exception 'throw 'my-error (: erlang throw 'my-error)))

(defun is-exception-wrong-class_test ()
  (try
    (progn
      (is-exception 'throw 'badarith (/ 1 0))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(defun is-exception-wrong-term_test ()
  (try
    (progn
      (is-exception 'error 'undef (/ 1 0))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(defun is-exception-unexpected-success_test ()
  (try
    (progn
      (is-exception 'error 'badarith (+ 1 1))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

; XXX add test: is-not-exception_test

(defun is-error_test ()
  (is-error 'badarith (/ 1 0)))

(defun is-error-wrong-term_test ()
  (try
    (progn
      (is-error 'undef (/ 1 0))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(defun is-error-unexpected-success_test ()
  (try
    (progn
      (is-error 'badarith (+ 1 1))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

; XXX add test: is-not-error_test

(defun is-throw_test ()
  (is-throw 'my-error (throw 'my-error)))

(defun is-throw-wrong-term_test ()
  (try
    (progn
      (is-throw 'my-error (: erlang throw 'another-error))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(defun is-throw-unexpected-success_test ()
  (try
    (progn
      (is-throw 'my-error (list 'no 'problem 'here))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

; XXX add test: is-not-throw_test

(defun is-exit_test ()
  (is-exit 'my-error (exit 'my-error)))

(defun is-exit-wrong-term_test ()
  (try
    (progn
      (is-exit 'my-error (: erlang exit 'another-error))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(defun is-exit-unexpected-success_test ()
  (try
    (progn
      (is-exit 'my-error (list 'no 'problem 'here))
      (: erlang error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

; XXX add test: is-not-exit_test

; XXX add test: is-match_test
; XXX add test: is-match-fail_test

; XXX add test: is-not-match_test
; XXX add test: is-not-match-fail_test
