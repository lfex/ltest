(defmodule ltest-unit
  (export
   (behaviour_info 1)
   (get-modules 0)))

(defun behaviour_info
  (('callbacks)
    '())
  ((_)
    'undefined))

(defun get-modules ()
  'ok)
