; This macro returns the boilerplate needed for every assertion's failure
; cases.
(defmacro DEFAULT-DATA ()
  `(list
     (tuple 'module (MODULE))
     (tuple 'line (LINE))))