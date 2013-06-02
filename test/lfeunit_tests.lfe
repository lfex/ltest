(defmodule lfeunit_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2)
      (assert-exception 3)
      (assert-error 2)
      (assert-throw 2)
      (assert-exit 2))))

(defun assert_test ()
  (assert `'true)
  (assert '(not 'false))
  (assert '(not (not 'true))))

(defun assert-fail_test ()
  (try
    (progn
      (assert `'false)
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-failed-assert value `'assert_failed)))))

(defun assert-not_test ()
  (assert-not `'false)
  (assert-not '(not 'true))
  (assert-not '(not (not 'false))))

(defun assert-not-fail_test ()
  (try
    (progn
      (assert-not `'true)
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-failed-assert value `'assert-not_failed)))))

(defun assert-equal_test ()
  (assert-equal 1 1)
  (assert-equal 1 '(+ 1 0))
  (assert-equal 1 '(- 2 1)))

(defun assert-equal-fail_test ()
  (try
    (progn
      (assert-equal 1 2)
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-failed-assert value `'assert-equal_failed)))))

(defun assert-not-equal_test ()
  (assert-not-equal 0 1)
  (assert-not-equal 0 '(+ 1 0))
  (assert-not-equal 0 '(- 2 1)))

(defun assert-not-equal-fail_test ()
  (try
    (progn
      (assert-not-equal 1 '(+ 1 0))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-failed-assert value `'assert-not-equal_failed)))))

(defun assert-exception_test ()
  (assert-exception 'error 'badarith '(/ 1 0)))

(defun assert-exception-wrong-class_test ()
  (try
    (progn
      (assert-exception 'throw 'badarith '(/ 1 0))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-class)))))

(defun assert-exception-wrong-term_test ()
  (try
    (progn
      (assert-exception 'error 'undef '(/ 1 0))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

(defun assert-exception-unexpected-success_test ()
  (try
    (progn
      (assert-exception 'error 'badarith '(+ 1 1))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-success)))))

; XXX add test: assert-not-exception_test

(defun assert-error_test ()
  (assert-error 'badarith '(/ 1 0)))

(defun assert-error-wrong-term_test ()
  (try
    (progn
      (assert-error 'undef '(/ 1 0))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

(defun assert-error-unexpected-success_test ()
  (try
    (progn
      (assert-error 'badarith '(+ 1 1))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-success)))))

; XXX add test: assert-not-error_test

(defun assert-throw_test ()
  (assert-throw 'my-error '(: erlang throw 'my-error)))

(defun assert-throw-wrong-term_test ()
  (try
    (progn
      (assert-throw 'my-error '(: erlang throw 'another-error))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

(defun assert-throw-unexpected-success_test ()
  (try
    (progn
      (assert-throw 'my-error '(list 'no 'problem 'here))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-success)))))

; XXX add test: assert-not-throw_test

(defun assert-exit_test ()
  (assert-exit 'my-error '(: erlang exit 'my-error)))

(defun assert-exit-wrong-term_test ()
  (try
    (progn
      (assert-exit 'my-error '(: erlang exit 'another-error))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

(defun assert-exit-unexpected-success_test ()
  (try
    (progn
      (assert-exit 'my-error '(list 'no 'problem 'here))
      (: erlang error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-success)))))

; XXX add test: assert-not-exit_test

; XXX add test: assert-match_test
; XXX add test: assert-match-fail_test

; XXX add test: assert-not-match_test
; XXX add test: assert-not-match-fail_test
