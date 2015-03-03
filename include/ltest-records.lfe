(defrecord state
  (status (orddict:new))
  test-type
  (ok 0)
  (fail 0)
  (err 0)
  (skip 0)
  (cancel 0)
  (time 0))
