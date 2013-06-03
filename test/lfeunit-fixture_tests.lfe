(defmodule lfeunit-fixture_tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))
    (from lfeunit
      (assert 1)
      (_assert 1)
      (double 1)
      (assert-not 1)
      (assert-equal 2)
      (assert-not-equal 2)
      (assert-exception 3)
      (assert-error 2)
      (assert-throw 2)
      (assert-exit 2))))

(defun set-up ()
  'ok)

(defun tear-down (set-up-result)
  (assert-equal set-up-result `'ok))

(defun setup-test-case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  (list
    (lambda ()
      (assert-equal set-up-result 'nok))
    (lambda ()
      (assert-not-equal 'this-test 'very-silly))))

(defun foreach-test-case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  (list
    (lambda ()
      (assert-equal set-up-result 'ok))
    (lambda ()
      (assert-not-equal 'this-test 'very-silly))))

(defun setup-setup_test ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x) (setup-test-case x))))

(defun setup-setup-cleanup_test ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x) (tear-down x))
    (lambda (x) (setup-test-case x))))

; XXX add a test for setup-where-setup
; XXX add a test for setup-where-setup-cleanup


(defun foreach-setup_test ()
  (tuple
    'foreach
    (lambda () (set-up))
    (list
      (lambda (x) (setup-test-case x))
      (lambda (x) (foreach-test-case x)))))

(defun foreach-setup-cleanup_test ()
  (tuple
    'foreach
    (lambda () (set-up))
    (lambda (x) (tear-down x))
    (list
      (lambda (x) (setup-test-case x))
      (lambda (x) (foreach-test-case x)))))

; XXX add a test for foreach-where-setup
; XXX add a test for foreach-where-setup-cleanup

; XXX add test with spawn option
; XXX add test with timeout option
; XXX add test with inorder option
; XXX add test with inparallel option

(defun test-1_test_ ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x)
      (_assert '"apple"))))

(defun test-2_test_ ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x)
      (_assert 1))))

(defun test-3_test_ ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x)
      (_assert `'false))))

(defun test-4_test_ ()
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x) (tear-down x))
    (lambda (x)
      (_assert `'true))))

(defun test-5_test ()
  (assert-equal 5 '(_assert '"apple")))

(defun test-6_test ()
  (assert-equal 5 '(double 2)))