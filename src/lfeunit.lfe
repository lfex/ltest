(defmodule lfeunit
  (export all))

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
;  (let ((name 'assert-exception_failed)
;        (data (list (tuple 'module '"module")
;                    (tuple 'line '"line")
;                    (tuple 'expression '"expression")
;                    (tuple 'pattern '"pattern")
;                    (tuple 'unexpected_success '"X"))))
;    (try (eval expression)
;      (list 'oops)
;      (catch
;        ((tuple type value ignore;)
;         (: io format '"type: ~p; value: ;~p; ignore: ~p~n"
;           (list type value ignore))))))
 'ok)

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