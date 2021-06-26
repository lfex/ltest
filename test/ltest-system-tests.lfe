(defmodule ltest-system-tests
  (behaviour ltest-system)
  (export all))

(include-lib "include/ltest-macros.lfe")

(deftest testing-behaviour-use-true
  (is 'true))

(deftest testing-behaviour-use-false
  (is-not 'false))
