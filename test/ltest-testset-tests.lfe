(defmodule ltest-testset-tests
  (behaviour ltest-unit)
  (export all)
  (import
    (from ltest
      (check-failed-assert 2))))

(include-lib "include/ltest-macros.lfe")

(deftest testset-with-one `[,(is 'true)])

(deftest testset-with-two `[,(is 'true) ,(is-not 'false)])

(deftest testset-with-three `[,(is 'true) ,(is-not 'false) ,(is-equal 2 2)])

(deftest testset-nested
  `[,(is 'true)
    ,(is-not 'false)
    ,(is-equal 2 2)
    [,(is 'true) ,(is-not 'false) ,(is-equal 1 1)]])

(deftest testset-deeply-nested
  `[,(is 'true)
    ,(is-not 'false)
    ,(is-equal 1 1)
    [,(is 'true)
     ,(is-not 'false)
     ,(is-equal 2 2)
     [,(is 'true)
      ,(is-not 'false)
      ,(is-equal 3 3)
      ,(try
         (progn
           (is-equal 3 4)
           (error 'unexpected-test-succes))
         (catch
           (`#(,type ,value ,_)
            (check-failed-assert value (assert-equal-failed)))))]]])
