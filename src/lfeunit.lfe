(defmodule lfeunit
  (export all)
  (import (from erlang (atom_to_list 1))))

(defun add-data (key value data-1)
  "A utility function for appending to assert* result data."
  (++ data-1 (list (tuple key value))))

(defun check-wrong-assert-exception (data expected)
  "
  This function
    1) unwraps the data held in the error result returned by
       assert-exception when an unexpected error occurs, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same
  "
  (let (((tuple 'assert-exception_failed
    (list _ _ _ _ (tuple fail-type _))) data))
    (assert-equal fail-type expected)))

(defun assert (bool-expression)
  "takes an expression that returns a boolean value"
  (let ((check (not (not bool-expression)))
        (name 'assert-equal_failed)
        (data (list (tuple 'module '"module")
                    (tuple 'line '"line")
                    (tuple 'expression '"expression")
                    (tuple 'expected 'true)
                    (tuple 'value bool-expression))))
    (case check
      ('true 'ok)
      ('false (: erlang error (tuple name data))))))


(defun assert-not (bool-expression)
  "takes an expression that returns a boolean value"
  'true)

(defun assert-equal (expected expression)
  ""
  (cond
    ((== expected (eval expression))
     'ok)
    ((/= expected (eval expression))
      (: erlang error '"oops"))))

(defun assert-not-equal (expected expression)
  ""
  'ok)

(defun assert-exception (expected-class expected-term expression)
  ""
  (let* ((fail 'assert-exception_failed)
         (succeed 'unexpected-success)
         (pattern (++ '"{ " (atom_to_list expected-class)
                    '" , " (atom_to_list expected-term) '" , [...] }"))
         (data (list (tuple 'module '"module")
                     (tuple 'line '"line")
                     (tuple 'expression expression)
                     (tuple 'pattern pattern))))
    (try
      (progn
        (eval expression)
        (: erlang error succeed))
      (catch
        ((tuple actual-class actual-term _)
          (cond
            ; did we get the expected exception class and term?
            ((and (== actual-class expected-class)
                  (== actual-term expected-term))
              'ok)
            ; did we get a success eval instead of an exception?
            ((== actual-term succeed)
              (: erlang error
                (tuple fail
                  (add-data succeed (eval expression) data))))
            ; did we get the wrong exception class?
            ((/= actual-class expected-class)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-exception-class
                    (tuple expected-class expected-term
                      (: erlang get_stacktrace)) data))))
            ; did we get the wrong exception term?
            ((/= actual-term expected-term)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-exception-term
                    (tuple expected-class expected-term
                      (: erlang get_stacktrace)) data))))))))))

(defun assert-not-exception (expected-class expected-term expression)
  ""
  'true)

(defun assert-error (expected-term expression)
  ""
  (assert-exception 'error expected-term expression))

(defun assert-not-error (expected-term expression)
  ""
  (assert-not-exception 'error expected-term expression))

(defun assert-exit (expected-term expression)
  ""
  (assert-exception 'exit expected-term expression))

(defun assert-not-exit (expected-term expression)
  ""
  (assert-not-exception 'exit expected-term expression))

(defun assert-throw (expected-term expression)
  ""
  (assert-exception 'throw expected-term expression))

(defun assert-not-throw (expected-term expression)
  ""
  (assert-not-exception 'throw expected-term expression))

(defun assert-match (guard expression)
  ""
  'true)

(defun assert-not-match (guard expression)
  ""
  'true)