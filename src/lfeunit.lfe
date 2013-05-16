(defmodule lfeunit
  (export all))

(defun assert (bool-expression)
  "takes an expression that returns a boolean value"
  'true)

(defun assert-not (bool-expression)
  "takes an expression that returns a boolean value"
  'true)

(defun assert-equal (expected, expression)
  ""
  'ok)

(defun assert-not-equal (expected, expression)
  ""
  (funcall (lambda (__x)

  (: erlang error
    (tuple 'assert-not-equal_failed
      (list (tuple 'module 'x)
            (tuple 'line 'x)
            (tuple 'expression 'x)
            (tuple 'expected 'x)
            (tuple 'value 'x')))))

    ) expected)

(defun assert-exception (class term expression)
  ""
  'true)

(defun assert-not-exception (class term expression)
  ""
  'true)

(defun assert-match ()
  ""
  'true)

(defun assert-not-match ()
  ""
  'true)