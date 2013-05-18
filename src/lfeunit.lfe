(defmodule lfeunit
  (export all)
  (import (from erlang (atom_to_list 1))))

(defun add-data (key value data-1)
  (++ data-1 (list (tuple key value))))

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
  'ok)

(defun assert-not-equal (expected expression)
  ""
  'ok)

(defun assert-exception (class term expression)
  ""
  (let* ((fail 'assert-exception_failed)
         (succeed 'unexpected-success)
         (pattern (++ '"{ " (atom_to_list class)
                    '" , " (atom_to_list term) '" , [...] }"))
         (data (list (tuple 'module '"module")
                     (tuple 'line '"line")
                     (tuple 'expression expression)
                     (tuple 'pattern pattern))))
    (try
      (progn
        (eval expression)
        (: erlang error succeed))
      (catch
        ((tuple type value ignore)
          (cond
            ((and (== type class) (== value term))
              'ok)
            ((== value succeed)
              (: erlang error
                (tuple fail
                  (add-data succeed (eval expression) data))))
            ((/= type class)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-exception-type
                    (tuple class term (: erlang get_stacktrace)) data))))
            ((/= value term)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-error-type
                    (tuple class term (: erlang get_stacktrace)) data))))))))))

(defun assert-not-exception (class term expression)
  ""
  'true)

(defun assert-error (term expression)
  ""
  (assert-exception 'error term expression))

(defun assert-not-error (term expression)
  ""
  (assert-not-exception 'error term expression))

(defun assert-exit (term expression)
  ""
  (assert-exception 'exit term expression))

(defun assert-not-exit (term expression)
  ""
  (assert-not-exception 'exit term expression))

(defun assert-throw (term expression)
  ""
  (assert-exception 'throw term expression))

(defun assert-not-throw (term expression)
  ""
  (assert-not-exception 'throw term expression))

(defun assert-match (guard expression)
  ""
  'true)

(defun assert-not-match (guard expression)
  ""
  'true)