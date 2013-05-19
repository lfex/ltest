;;;; This module is simply here to test that one can use lfeunit as a module in
;;;; addition to the standard way of using it: as an include file.


(defmodule lfeunit_tests
  (export all)
  (import (from lfeunit (assert 1) (assert-not 1) (assert-equal 2))))

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