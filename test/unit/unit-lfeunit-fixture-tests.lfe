(defmodule unit-lfeunit-fixture-tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "include/lfeunit-macros.lfe")


(defun set-up ()
  'ok)

(defun tear-down (set-up-result)
  (is-equal set-up-result 'ok))

(defun setup-test-case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  (list
    (lambda ()
      (is-equal set-up-result 'nok))
    (lambda ()
      (is-not-equal 'this-test 'very-silly))))

(defun foreach-test-case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  (list
    (lambda ()
      (is-equal set-up-result 'ok))
    (lambda ()
      (is-not-equal 'this-test 'very-silly))))

(deftestskip setup-setup
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x) (setup-test-case x))))

(deftestskip setup-setup-cleanup
  (tuple
    'setup
    (lambda () (set-up))
    (lambda (x) (tear-down x))
    (lambda (x) (setup-test-case x))))

; XXX add a test for setup-where-setup
; XXX add a test for setup-where-setup-cleanup

(deftestskip foreach-setup
  (tuple
    'foreach
    (lambda () (set-up))
    (list
      (lambda (x) (setup-test-case x))
      (lambda (x) (foreach-test-case x)))))

(deftestskip foreach-setup-cleanup
  (tuple
    'foreach
    (lambda () (set-up))
    (lambda (x) (tear-down x))
    (list
      (lambda (x) (setup-test-case x))
      (lambda (x) (foreach-test-case x)))))

; XXX add a test for foreach-where-setup
; XXX add a test for foreach-where-setup-cleanup

; XXX add a test for node
; XXX add a test for node-args

; XXX add a test for foreachx-setupx-pairs
; XXX add a test for foreachx-wherex-setupx-pairs
; XXX add a test for foreachx-setupx-cleanupx-pairs
; XXX add a test for foreachx-wherex-setupx-cleanupx-pairs

; XXX add test with spawn option
; XXX add test with timeout option
; XXX add test with inorder option
; XXX add test with inparallel option
