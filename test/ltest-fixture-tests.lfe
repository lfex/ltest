(defmodule ltest-fixture-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "include/ltest-macros.lfe")

(defun set-up () 'ok)

(defun tear-down (set-up-result) (is-equal set-up-result 'ok))

(defun setup_test_case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  `[,(is-equal* set-up-result 'ok)
    ,(is-not-equal* 'this-test 'very-silly)])

(defun foreach_test_case (set-up-result)
  "This is called the 'Instantiator' in EUnit parlance."
  `[,(is-equal* set-up-result 'ok)
    ,(is-not-equal* 'this-test 'very-silly)])

(deftestgen setup-setup `#(setup ,(defsetup set-up) ,#'setup_test_case/1))

(deftestgen setup-setup-cleanup
  `#(setup ,(defsetup set-up) ,(defteardown tear-down) ,#'setup_test_case/1))

; XXX add a test for setup-where-setup
; XXX add a test for setup-where-setup-cleanup

(deftestgen foreach-setup
  `#(foreach ,(defsetup set-up) [,#'setup_test_case/1 ,#'foreach_test_case/1]))

(deftestgen foreach-setup-cleanup
  `#(foreach ,(defsetup set-up) ,(defteardown tear-down)
             [,#'setup_test_case/1 ,#'foreach_test_case/1]))

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
