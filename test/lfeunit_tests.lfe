(defmodule lfeunit_tests
  (export all)
  (import (from lfeunit (assert-equal 2))))

(defun assert-equal_test ()
  (assert-equal 1 1))