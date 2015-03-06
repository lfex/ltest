(defmodule ltest-fixturecase-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "include/ltest-macros.lfe")

(defun set-up ()
  'ok)

(defun tear-down (set-up-result)
  (is-equal set-up-result 'ok))

(deftestcase setup-tc (set-up-result)
  ;; This is called the 'Instantiator' in EUnit parlance.
  (is-equal set-up-result 'ok)
  (is-not-equal 'this-test 'very-silly))

(deftestcase foreach-tc (set-up-result)
  ;; This is called the 'Instantiator' in EUnit parlance.
  (is-equal set-up-result 'ok)
  (is-not-equal 'this-test 'very-silly))

(deftestgen setup-setup
  (tuple
    'setup
    (defsetup set-up)
    (deftestcases
      setup-tc)))

(deftestgen setup-setup-cleanup
  (tuple
    'setup
    (defsetup set-up)
    (defteardown tear-down)
    (deftestcases
      setup-tc)))

; XXX add a test for setup-where-setup
; XXX add a test for setup-where-setup-cleanup

(deftestgen foreach-setup
  (tuple
    'foreach
    (defsetup set-up)
    (deftestcases
      setup-tc
      foreach-tc)))

(deftestgen foreach-setup-cleanup
  (tuple
    'foreach
    (defsetup set-up)
    (defteardown tear-down)
    (deftestcases
      setup-tc
      foreach-tc)))

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
