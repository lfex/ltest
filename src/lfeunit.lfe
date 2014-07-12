;; This module isn't necessary at all; however, it does provide a convenience
;; in the event that you want to do some debugging from the LFE REPL. Since
;; one can't include macros directly in the REPL, this module allows one to
;; still use the macros while in the REPL by doing the following:
;;
;; > (slurp '"src/lfeunit.lfe")
;;
;; At which point you will be able to call all the macros:
;;
;; > (is-equal 1 1)
;; ok
;; > (is-equal 1 2)
;; exception error: #(assertEqual_failed
;;                    (#(module lfeunit)
;;                     #(line 1)
;;                     #(expression "2")
;;                     #(expected 1)
;;                     #(value 2)))
;;
(defmodule lfeunit
  (export all))

(include-lib "include/lfeunit-macros.lfe")

(defun check-failed-assert (data expected)
  "This function
    1) unwraps the data held in the error result returned by a failed
       assertion, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let (((tuple failure-type _) data))
    (is-equal failure-type expected)))

(defun check-wrong-assert-exception (data expected)
  "This function
    1) unwraps the data held in the error result returned by
       assert-exception when an unexpected error occurs, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let (((tuple 'assertException_failed
    (list _ _ _ _ (tuple fail-type _))) data))
    (is-equal fail-type expected)))
