(defmodule ltest-named-tests
  (behaviour ltest-unit)
  (import
    (from ltest
      (check-failed-assert 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest named-is
  (tuple '"Testing the 'is' assertion macro."
    (is 'true)
    (is (not 'false))
    (is (not (not 'true)))))

(deftest named-is-not-fail
  (tuple '"Testing the 'is-not' assertion under failing conditions."
    (try
      (progn
        (is-not 'true)
        (error 'unexpected-test-success))
      (catch ((tuple type value _)
        (check-failed-assert value (assertion-failed)))))))

(deftest named-testset-with-one
  (tuple '"Testing a named test set with one entry."
    (list (is 'true))))

(deftest named-testset-with-two
  (tuple '"Testing a named test set with two entries."
    (list (is 'true)
          (is-not 'false))))

(deftest named-testset-with-three
  (tuple '"Testing a named test set with three entries."
    (list (is 'true)
          (is-not 'false)
          (is-equal 2 2))))

(deftest named-testset-nested
  (tuple '"Testing a named nested test set."
    (list (is 'true)
          (is-not 'false)
          (is-equal 2 2)
          (list (is 'true)
                (is-not 'false)
                (is-equal 1 1)))))

(deftest named-testset-deeply-nested
  (tuple '"Testing a named, deeply-nested test set."
    (list (is 'true)
          (is-not 'false)
          (is-equal 1 1)
          (list (is 'true)
                (is-not 'false)
                (is-equal 2 2)
                (list (is 'true)
                      (is-not 'false)
                      (is-equal 3 3)
                      (try
                        (progn
                          (is-equal 3 4)
                          (error 'unexpected-test-succes))
                        (catch
                          ((tuple type value _)
                           (check-failed-assert value
                                                (assert-equal-failed))))))))))
