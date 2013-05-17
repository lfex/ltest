(defmodule ae
  (export all))

;(include-file "macro_play.lfm")

(defmacro backq (expr)
  (backq-expand expr))

(eval-when-compile
  (defun backq-expand (expr)
    (: io format '"expr: ~p~n" (list expr))))

; assertEqual(Expect, Expr) ->
;     ((fun (__X) ->
;         case (Expr) of
;         __X -> ok;
;         __V -> .erlang:error({assertEqual_failed,
;                       [{module, ?MODULE},
;                        {line, ?LINE},
;                        {expression, (Expr)},
;                        {expected, __X},
;                        {value, __V}]})
;         end
;       end)(Expect)).

(defun assert-equal1 (expected expression)
  (funcall
    (lambda (x)
      (case expression
        (x 'ok)
        (v (: erlang error 'oops))))
    expected))

(defun assert-equal2 (expected expression)
  (: io format '"expected: ~p~nexpression: ~p~n~n" (list expected expression))
  (case expression
    (expected 'ok)
    (v (: erlang error 'oops))))

(defun ae3 (expected expression)
  (backq expected)
  (backq expression))