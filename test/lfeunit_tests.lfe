(defmodule lfeunit_tests
  (export all))

(defmacro MODULE () `'lfeunit_tests)

(include-lib "src/lfeunit.lfe")

(defun assert_test ()
  (assert `'true)
  (assert '(not 'false))
  (assert '(not (not 'true))))

;(defun assert-fail_test ()
;  (assert-error XXX (assert 'false))

(defun assert-not_test ()
  (assert-not `'false)
  (assert-not '(not 'true))
  (assert-not '(not (not 'false))))

(defun assert-equal_test ()
  (assert-equal 1 1)
  (assert-equal 1 '(+ 1 0))
  (assert-equal 1 '(- 2 1)))

(defun assert-equal-fail_test ()
  (try
    (progn
     (assert-equal 1 2))
    (catch ((tuple type value _)
      (check-failed-assert value `'assert-equal_failed)))))

;(defun assert-not-equal-fail_test ()
;  (assert-equal 1 1))

;(defun assert-not-equal_test ()
;  (assert-equal 1 2))

(defun assert-exception_test ()
  (assert-exception 'error 'badarith '(/ 1 0)))

(defun assert-exception-wrong-class_test ()
  (try
    (progn
      (assert-exception 'throw 'badarith '(/ 1 0)))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-class)))))

(defun assert-exception-wrong-term_test ()
  (try
    (progn
      (assert-exception 'error 'undef '(/ 1 0)))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

(defun assert-exception-unexpected-success_test ()
  (try
    (progn
      (assert-exception 'error 'badarith '(+ 1 1)))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-success)))))

; XXX add test: assert-exception-fail_test

; XXX add test: assert-not-exception_test

(defun assert-error_test ()
  (assert-error 'badarith '(/ 1 0)))

(defun assert-error-wrong-term_test ()
  (try
    (progn
      (assert-error 'undef '(/ 1 0)))
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