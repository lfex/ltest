(defmodule assert_equal
  (export all))

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
  (: io format '"expected: ~p~nexpression: ~p~n~n" (list expected expression))
  (case expression
    (expected 'ok)
    (v (: erlang error 'oops))))

(defmacro xxx (y z)
  `(list y z ,y ,z))

(defun xx (y z)
  (xxx y z))