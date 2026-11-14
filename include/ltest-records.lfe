(defrecord state
  status
  test-type
  color?
  (ok 0)
  (fail 0)
  (err 0)
  (skip 0)
  (cancel 0)
  (time 0))

(defun --loaded-ltest-records-- ()
  "Return `ok`. This dummy function marks the end of the include so the REPL
   prints its name as the last function loaded."
  'ok)
