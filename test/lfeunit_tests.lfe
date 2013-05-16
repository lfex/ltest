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
      (assert-match 2)
      (assert-not-match 2))))

(defun assert-equal-false_test ()
  (assert-equal 1 2))

(defun assert-equal-true_test ()
  (assert-equal 1 1))

(defun assert-not-equal-false_test ()
  (assert-equal 1 1))

(defun assert-not-equal-true_test ()
  (assert-equal 1 2))