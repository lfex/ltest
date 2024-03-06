(defmodule ltest-skipped-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftestskip bogus-test-will-be-skipped
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))
