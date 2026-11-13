(defmodule ltest-basic-tests
  (behaviour ltest-unit)
  (import
    (from ltest
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest is
  (is 'true)
  (is (not 'false))
  (is (not (not 'true))))

(deftest are*
  (are* (x y) (== x y)
    2 (+ 1 1)
    4 (* 2 2)))

(deftest are-fail
  (try
    (progn
      (are* (x y) (== x y) 2 3)
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assertion-failed))))))

(deftest is-with-one-phrase-deftest
  "This unit tests was originally testing the deftest macro with just one
  phrase."
  (is-not 'false))

(deftest is-with-two-phrase-deftest
  "This unit tests was originally testing the deftest macro with two phrases."
  (is-not 'false)
  (is 'true))

(deftest is-with-many-phrase-deftest
  "This unit tests was originally testing the deftest macro with several
  phrases."
  (is-not 'false)
  (is 'true)
  (is-equal 1 1)
  (is-equal 1 (+ 1 0))
  (is-not-equal 1 2))

(deftest is-fail
  (try
    (progn
      (is 'false)
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assertion-failed))))))

(deftest is-not
  (is-not 'false)
  (is-not (not 'true))
  (is-not (not (not 'false))))

(deftest is-not-fail
  (try
    (progn
      (is-not 'true)
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assertion-failed))))))

(deftest is-equal
  (is-equal 1 1)
  (is-equal 1 (+ 1 0))
  (is-equal 1 (- 2 1)))

(deftest is-equal-fail
  (try
    (progn
      (is-equal 1 2)
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assert-equal-failed))))))

(deftest is-not-equal
  (is-not-equal 0 1)
  (is-not-equal 0 (+ 1 0))
  (is-not-equal 0 (- 2 1)))

(deftest is-not-equal-fail
  (try
    (progn
      (is-not-equal 1 (+ 1 0))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assert-not-equal-failed))))))

(deftest is-exception
  (is-exception 'throw 'my-error (throw 'my-error)))

(deftest is-exception-wrong-class
  (try
    (progn
      (is-exception 'throw 'badarith (/ 1 0))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-exception-wrong-term
  (try
    (progn
      (is-exception 'error 'undef (/ 1 0))
      (error 'unexpected-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-exception-unexpected-success
  (try
    (progn
      (is-exception 'error 'badarith (+ 1 1))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

(deftest is-not-exception
  (is-not-exception (+ 2 2)))

(deftest is-not-exception-exit
  (is-not-exception 'exit 'badarith (/ 1 0)))

(deftest is-not-exception-throw
  (is-not-exception 'throw 'badarith (/ 1 0)))

(deftest is-error
  (is-error 'badarith (/ 1 0)))

(deftest is-error-wrong-term
  (try
    (progn
      (is-error 'undef (/ 1 0))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-error-unexpected-success
  (try
    (progn
      (is-error 'badarith (+ 1 1))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

(deftest is-not-error
  (is-not-error (+ 1 2))
  (is-not-error 'undef (/ 1 0)))

(deftest is-not-error-fail
  (try
    (progn
      (is-not-error 'badarith (/ 1 0))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-throw
  (is-throw 'my-error (throw 'my-error)))

(deftest is-throw-wrong-term
  (try
    (progn
      (is-throw 'my-error (throw 'another-error))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-throw-unexpected-success
  (try
    (progn
      (is-throw 'my-error (list 'no 'problem 'here))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

(deftest is-not-throw
  (is-not-throw (+ 1 2))
  (is-not-throw 'other (throw 'expected)))

(deftest is-not-throw-fail
  (try
    (progn
      (is-not-throw 'expected (throw 'expected))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-exit
  (is-exit 'my-error (exit 'my-error)))

(deftest is-exit-wrong-term
  (try
    (progn
      (is-exit 'my-error (exit 'another-error))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

(deftest is-exit-unexpected-success
  (try
    (progn
      (is-exit 'my-error (list 'no 'problem 'here))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_success)))))

(deftest is-not-exit
  (is-not-exit (+ 1 2))
  (is-not-exit 'other (throw 'expected)))

(deftest is-not-exit-fail
  (try
    (progn
      (is-not-exit 'expected (exit 'expected))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-wrong-assert-exception value 'unexpected_exception)))))

; XXX add test: is-match_test
(deftest is-match
  (is-match (tuple 1 'a) #(1 a))
  (is-match (tuple 1 (tuple 2 'pull)) #(1 #(2 pull))))

(deftest is-not-match
  (is-not-match (tuple 1 'a) #(1 b)))

(deftest is-not-match-fail
  (try
    (progn
      (is-not-match (tuple 1 'a) #(1 a))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value 'assertNotMatch)))))

(deftest is-match-fail
  (try
    (progn
      (is-match (tuple 1 'a) #(1 b))
      (error 'unexpected-test-success))
    (catch ((tuple type value _)
      (check-failed-assert value (assert-match-failed))))))
