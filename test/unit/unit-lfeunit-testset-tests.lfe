(defmodule unit-lfeunit-testset-tests
  (export all)
  (import
    (from lfeunit-util
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "include/lfeunit-macros.lfe")

(deftest testset-with-one
  (list (is 'true)))

(deftest testset-with-two
  (list (is 'true)
        (is-not 'false)))

(deftest testset-with-three
  (list (is 'true)
        (is-not 'false)
        (is-equal 2 2)))