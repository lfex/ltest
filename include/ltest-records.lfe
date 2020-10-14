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
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
