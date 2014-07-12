;;;; Utility functions
;;;;
(defmodule lfeunit-util
  (export all))

(include-lib "include/lfeunit-macros.lfe")

(defmacro DEFAULT-DATA ()
  "This macro returns the boilerplate needed for every assertion's failure
  cases."
  `(list
     (tuple 'module (MODULE))
     (tuple 'line (LINE))))

(defun add-data (key value data)
  "A utility function for appending to assert* result data."
  (++ data (list (tuple key value))))

(defun get-failure-data (expected expression)
  "Building upon the boilerplate data returned from DEFAULT-DATA, this function
  gets the rest of the data needed when returning results for a failed
  assertion."
  (let* ((value (eval expression))
         (expr-data (add-data 'expression expression (DEFAULT-DATA)))
         (expt-data (add-data 'expected expected expr-data)))
    (add-data 'value value expt-data)))

(defun get-exception-data (expected-class expected-term expression)
  "Building upon the boilerplate data returned from DEFAULT-DATA, this function
  gets the rest of the data needed when returning results for a failed
  exception assertion."
  (let ((pattern
          (++
            '"{ " (atom_to_list expected-class)
            '" , " (atom_to_list expected-term)
            '" , [...] }"))
       (expr-data (add-data 'expression expression (DEFAULT-DATA))))
    (add-data 'pattern pattern expr-data)))
