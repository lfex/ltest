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
  'true)

(defun assert-not-equal (expected, expression)
  ""
  'true)

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