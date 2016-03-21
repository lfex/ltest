(defmodule ltest-generated-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "include/ltest-macros.lfe")

(deftestgen is* (is* 'true))

(deftestgen is-not*-in-list `[,(is-not* 'false)])

(deftestgen many-generators-in-list
  `[,(is* 'true) ,(is-not* 'false) ,(is-equal* 1 1)])

(deftestgen nested-test-set
  `[,(is* 'true)
    ,(is-not* 'false)
    ,(is-equal* 2 2)
    [,(is* 'true) ,(is-not* 'false) ,(is-equal* 1 1)]])
