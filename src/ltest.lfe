(defmodule ltest
  (export all))

(include-lib "include/ltest-macros.lfe")

(defun run ()
  (run 'all))

(defun run
  (('all) (ltest-runner:all))
  ((type) (ltest-runner:run type)))

(defun run (apps type)
  (ltest-runner:run apps type))

(defun run (profile apps type)
  (ltest-runner:run profile apps type))

(defun get-test-beams (type)
  (get-test-beams (list (ltest-util:app-name)) type))

(defun get-test-beams (apps type)
  (get-test-beams (ltest-const:default-test-profile) apps type))

(defun get-test-beams (profile apps type)
  (lists:filter
    (get-predicate type)
    (ltest-beams:paths profile apps 'no-extension)))

(defun get-predicate (type)
  (case type
   ('integration #'integration?/1)
   ('system #'system?/1)
   ('unit #'unit?/1)
   (_ #(error test-type-not-found))))

(defun get-integration-beams ()
  (get-test-beams 'integration))

(defun get-integration-beams (apps)
  (get-test-beams apps 'integration))

(defun get-integration-beams (profile apps)
  (get-test-beams profile apps 'integration))

(defun get-system-beams ()
  (get-test-beams 'system))

(defun get-system-beams (apps)
  (get-test-beams apps 'system))

(defun get-system-beams (profile apps)
  (get-test-beams profile apps 'system))

(defun get-unit-beams ()
  (get-test-beams 'unit))

(defun get-unit-beams (apps)
  (get-test-beams apps 'unit))

(defun get-unit-beams (profile apps)
  (get-test-beams profile apps 'unit))

(defun has-behaviour? (beam type)
  (lists:member
    type
    (ltest-util:get-beam-behaviours beam)))

(defun integration? (beam)
  (has-behaviour? beam 'ltest-integration))

(defun system? (beam)
  (has-behaviour? beam 'ltest-system))

(defun unit? (beam)
  (has-behaviour? beam 'ltest-unit))

(defun check-skip-funcs (funcs)
  (lists:map
    (match-lambda
      (((tuple func arity))
        (case (re:run (atom_to_list func) (ltest-const:skip-test-patt))
          ((tuple 'match _) `#(,func ,arity))
          (_ 'false))))
    funcs))

(defun check-skipped-tests (funcs)
  (lists:map
    (match-lambda
      (((tuple func arity))
        (case (re:split (atom_to_list func)
                        (++ (ltest-coonst:skip-test-group-patt))
                        '(#(return list)))
          ((list '() test-name _ '()) test-name)
          (_ 'false))))
    funcs))

(defun get-skip-funcs (module)
  (ltest-util:filter-files
    #'check-skip-funcs/1
    (ltest-util:get-module-exports module)))

(defun get-skipped-tests (module)
  (ltest-util:filter-files
    #'check-skipped-tests/1
    (ltest-util:get-module-exports module)))

(defun check-failed-assert (data expected)
  "This function
    1) unwraps the data held in the error result returned by a failed
       assertion, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let ((`#(,failure-type ,_) data))
    (is-equal failure-type expected)))

(defun check-wrong-assert-exception (data expected)
  "This function
    1) unwraps the data held in the error result returned by
       assert-exception when an unexpected error occurs, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let* ((reason (assert-exception-failed))
         (`#(,reason (,_ ,_ ,_ ,_ #(,fail-type ,_))) data))
    (is-equal fail-type expected)))
