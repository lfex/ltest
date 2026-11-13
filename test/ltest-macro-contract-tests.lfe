(defmodule ltest-macro-contract-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun run-test-object (object)
  (eunit:test (list object) '(no_tty)))

(deftest delayed-not-match-polarity
  (is-equal 'ok
            (run-test-object (is-not-match* (tuple 1 'a) #(1 b))))
  (is-equal 'error
            (run-test-object (is-not-match* (tuple 1 'a) #(1 a)))))

(deftest delayed-not-exit-one-and-two-arguments
  (is-equal 'ok (run-test-object (is-not-exit* (/ 1 0))))
  (is-equal 'ok
            (run-test-object (is-not-exit* 'different (exit 'expected))))
  (is-equal 'error
            (run-test-object (is-not-exit* 'expected (exit 'expected))))
  (is-equal 'ok
            (run-test-object (is-not-exit* 'expected (throw 'expected)))))

(deftest delayed-throw-one-and-two-arguments
  (is-equal 'ok (run-test-object (is-throw* (throw 'expected))))
  (is-equal 'ok
            (run-test-object (is-throw* 'expected (throw 'expected))))
  (is-equal 'error
            (run-test-object (is-throw* 'different (throw 'expected)))))

(deftest delayed-not-throw-one-and-two-arguments
  (is-equal 'ok (run-test-object (is-not-throw* (+ 1 2))))
  (is-equal 'ok
            (run-test-object (is-not-throw* 'different (throw 'expected))))
  (is-equal 'error
            (run-test-object (is-not-throw* 'expected (throw 'expected))))
  (is-equal 'ok
            (run-test-object (is-not-throw* 'expected (exit 'expected)))))

(deftest delayed-expression-is-lazy-and-evaluated-once
  (let* ((caller (self))
         (object (is-equal* 'ok (progn (! caller 'once) 'ok))))
    (is-equal 'none
              (receive ('once 'once) (after 0 'none)))
    (is-equal 'ok (run-test-object object))
    (is-equal 'once
              (receive ('once 'once) (after 0 'none)))
    (is-equal 'none
              (receive (other other) (after 0 'none)))))

(deftest delayed-existing-assertion-families
  (is-equal 'ok (run-test-object (is* 'true)))
  (is-equal 'error (run-test-object (is* 'false)))
  (is-equal 'ok (run-test-object (is-not* 'false)))
  (is-equal 'error (run-test-object (is-not* 'true)))
  (is-equal 'ok (run-test-object (is-match* (tuple 1 'a) #(1 a))))
  (is-equal 'error (run-test-object (is-match* (tuple 1 'a) #(1 b))))
  (is-equal 'ok (run-test-object (is-equal* 1 (+ 0 1))))
  (is-equal 'error (run-test-object (is-equal* 1 2)))
  (is-equal 'ok (run-test-object (is-not-equal* 1 2)))
  (is-equal 'error (run-test-object (is-not-equal* 1 1)))
  (is-equal 'ok (run-test-object (is-exception* (throw 'expected))))
  (is-equal 'ok
            (run-test-object
              (is-exception* 'throw 'expected (throw 'expected))))
  (is-equal 'error
            (run-test-object
              (is-exception* 'error 'expected (throw 'expected))))
  (is-equal 'ok (run-test-object (is-not-exception* (+ 1 2))))
  (is-equal 'ok
            (run-test-object
              (is-not-exception* 'exit 'expected (throw 'expected))))
  (is-equal 'error
            (run-test-object
              (is-not-exception* 'throw 'expected (throw 'expected))))
  (is-equal 'ok (run-test-object (is-error* (/ 1 0))))
  (is-equal 'ok
            (run-test-object (is-error* 'badarith (/ 1 0))))
  (is-equal 'error
            (run-test-object (is-error* 'undef (/ 1 0))))
  (is-equal 'ok (run-test-object (is-not-error* (+ 1 2))))
  (is-equal 'ok
            (run-test-object (is-not-error* 'undef (/ 1 0))))
  (is-equal 'error
            (run-test-object (is-not-error* 'badarith (/ 1 0))))
  (is-equal 'ok (run-test-object (is-exit* (exit 'expected))))
  (is-equal 'ok
            (run-test-object (is-exit* 'expected (exit 'expected))))
  (is-equal 'error
            (run-test-object (is-exit* 'different (exit 'expected))))
  (is-equal 'ok (run-test-object (is-not-exit* (/ 1 0))))
  (is-equal 'error
            (run-test-object (is-not-exit* 'expected (exit 'expected))))
  (is-equal 'ok (run-test-object (is-throw* (throw 'expected))))
  (is-equal 'ok
            (run-test-object (is-throw* 'expected (throw 'expected))))
  (is-equal 'error
            (run-test-object (is-throw* 'different (throw 'expected))))
  (is-equal 'ok (run-test-object (is-not-throw* (+ 1 2))))
  (is-equal 'error
            (run-test-object (is-not-throw* 'expected (throw 'expected)))))
