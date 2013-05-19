(defmodule lfeunit_include_tests
  (export all))

; Define some macro/constants to make up for LFE's lack of ?MODULE and ?LINE
; support.
(defmacro MODULE () `'lfeunit_tests)
(defmacro LINE () `'unknown)

(include-lib "include/lfeunit.lfe")

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

; XXX add test: assert-exception-fail_test

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

; XXX add test: assert-error-unexpected-success_test
; XXX add test: assert-error-fail_test
;(defun assert-error-fail_test ()
;  (assert-not-error 'badarith '(+ 1 1)))

; XXX add test: assert-not-error_test

; XXX add test: assert-throw_test
; XXX add test: assert-throw-wrong-term_test
; XXX add test: assert-throw-unexpected-success_test
; XXX add test: assert-throw-fail_test

; XXX add test: assert-not-throw_test

; XXX add test: assert-exit_test
; XXX add test: assert-exit-wrong-term_test
; XXX add test: assert-exit-unexpected-success_test
; XXX add test: assert-exit-fail_test

; XXX add test: assert-not-exit_test

; XXX add test: assert-match_test
; XXX add test: assert-match-fail_test