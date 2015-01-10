(defrecord state
  (status (orddict:new))
  options
  (ok 0)
  (fail 0)
  (skip 0)
  (cancel 0)
  (time 0))
