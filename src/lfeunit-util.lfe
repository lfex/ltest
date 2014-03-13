;;;; Utility functions
;;;;
(defmodule lfeunit-util
  (export all))

(include-lib "include/lfeunit-macros.lfe")


;(defmacro LINE ()
;  "Define a macro/constant to make up for LFE's lack of ?LINE support."
;  1)

(defmacro DEFAULT-DATA ()
  "
  This macro returns the boilerplate needed for every assertion's failure
  cases.
  "
  `(list
     (tuple 'module (MODULE))
     (tuple 'line (LINE))))

(defmacro -test (expression)
  "A macro that allows expression names to end in -test for tests."
  (lambda () expression))

(defmacro _test (expression)
  "A macro that allows expression names to end in _test for tests."
  (lambda () expression))

(defun add-data (key value data)
  "A utility function for appending to assert* result data."
  (++ data (list (tuple key value))))

(defun check-failed-assert (data expected)
  "
  This function
    1) unwraps the data held in the error result returned by a failed
       assertion, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same.
  "
  (let (((tuple failure-type _) data))
    (is-equal failure-type expected)))

(defun check-wrong-assert-exception (data expected)
  "
  This function
    1) unwraps the data held in the error result returned by
       assert-exception when an unexpected error occurs, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same.
  "
  (let (((tuple 'assertException_failed
    (list _ _ _ _ (tuple fail-type _))) data))
    (is-equal fail-type expected)))

(defun get-failure-data (expected expression)
  "
  Building upon the boilerplate data returned from DEFAULT-DATA, this function
  gets the rest of the data needed when returning results for a failed
  assertion.
  "
  (let* ((value (eval expression))
         (expr-data (add-data 'expression expression (DEFAULT-DATA)))
         (expt-data (add-data 'expected expected expr-data)))
    (add-data 'value value expt-data)))

(defun get-exception-data (expected-class expected-term expression)
  "
  Building upon the boilerplate data returned from DEFAULT-DATA, this function
  gets the rest of the data needed when returning results for a failed
  exception assertion.
  "
  (let ((pattern
          (++
            '"{ " (atom_to_list expected-class)
            '" , " (atom_to_list expected-term)
            '" , [...] }"))
       (expr-data (add-data 'expression expression (DEFAULT-DATA))))
    (add-data 'pattern pattern expr-data)))
