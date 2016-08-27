(defmodule ltest-cancel-tests
  ;Take the number one out of the next line
  ;and  run make check-runner-ltest
  ; you should get one errored test and
  ; a message saying
  ; "One or more tests were cancelled."
  (behaviour ltest-unit1))

(include-lib "include/ltest-macros.lfe")

(defun set-up () 'ok)

(defun setup_test_case (set-up-result)
  "This test causes cancellation because of missing comma."
  `[ (is-not-equal* 'this-causes-cancellation 'because-of-comma-missing)
     ,(tuple "Another unused test" (is-not-equal* 'to-be 'or-not-to-be))])

(deftestgen setup-setup
  `#(setup ,(defsetup set-up) ,#'setup_test_case/1))

