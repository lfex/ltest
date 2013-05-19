(defmodule lfeunit_tests
  (export all)
  (import
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2)
      (assert-exception 3)
      (assert-not-exception 3)
      (assert-error 2)
      (assert-not-error 2)
      (assert-match 2)
      (assert-not-match 2)
      (check-wrong-assert-exception 2))))

(defun assert-succeed_test ()
  (assert 'true)
  (assert (not (not 'true))))

;(defun assert-equal-false_test ()
;  (assert-equal 1 2))

(defun assert-equal-true_test ()
  (assert-equal 1 1))

;(defun assert-not-equal-false_test ()
;  (assert-equal 1 1))

;(defun assert-not-equal-true_test ()
;  (assert-equal 1 2))

(defun assert-exception-succeed_test ()
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

;(defun assert-error-fail_test ()
;  (assert-not-error 'badarith '(+ 1 1)))

(defun assert-error-succeed_test ()
  (assert-error 'badarith '(/ 1 0)))

(defun assert-error-wrong-term_test ()
  (try
    (progn
      (assert-error 'undef '(/ 1 0)))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value `'unexpected-exception-term)))))

;(defun assert-fail_test ()
;  (assert-error XXX (assert 'false))