(defmodule lfeunit_tests
  (export all)
  (import
    (from lfeunit
      (assert 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2)
      (assert-error 2)
      (assert-not-error 2)
      (assert-match 2)
      (assert-not-match 2))))

;(defun assert-error-fail_test ()
;  (assert-not-error 'badarith (+ 1 1)))

(defun assert-error-succeed_test ()
  (assert-error 'badarith (/ 1 0)))

(defun assert-error-wrong-type_test ()
  (assert-error 'undef (/ 1 0)))

;(defun assert-fail_test ()
;  (assert-error XXX (assert 'false))

(defun assert-succeed_test ()
  (assert 'true)
  (assert (not (not 'true))))

(defun assert-equal-false_test ()
  (assert-equal 1 2))

(defun assert-equal-true_test ()
  (assert-equal 1 1))

(defun assert-not-equal-false_test ()
  (assert-equal 1 1))

(defun assert-not-equal-true_test ()
  (assert-equal 1 2))