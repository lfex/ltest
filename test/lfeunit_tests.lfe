(defmodule lfeunit_tests
  (export all)
  (import (from lfeunit (assert-equal 2))))

(defun assert-equal-false_test ()
  (assert-equal 1 2))

(defun assert-equal-true_test ()
  (assert-equal 1 1))