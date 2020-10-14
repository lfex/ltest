(defmodule ltest-runner
  (export all))

(include-lib "include/ltest-records.lfe")

(defun all ()
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (unit 'combined)
    (integration 'combined)
    (system 'combined)
    (ltest-formatter:test-suite-footer state)
    ;; Add support for third-party test runners here
    (case (code:which 'lse)
      ('non_existing (ltest-runner:run-beams 'selenium '()))
      (_ (lse-runner:selenium 'combined)))
    (ltest-formatter:test-suite-footer state)))

(defun integration (_)
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Integration Tests" state)
    (run 'integration)))

(defun integration ()
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (integration 'solo)
    (ltest-formatter:test-suite-footer state)))

(defun system (_)
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "System Tests" state)
    (run 'system)))

(defun system ()
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (system 'solo)
    (ltest-formatter:test-suite-footer state)))

(defun unit (_)
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Unit Tests" state)
    (run 'unit)))

(defun unit ()
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (unit 'solo)
    (ltest-formatter:test-suite-footer state)))

(defun run
  (('integration)
    (ltest-util:rebar-debug "Running integration tests ...")
    (let* ((`#(ok ,cwd) (file:get_cwd))
           (beams (ltest:get-integration-beams cwd)))
      (ltest-util:rebar-debug "Got cwd: ~p" `(,cwd))
      (ltest-util:rebar-debug "Got beams: ~p" `(,beams))
      (run-beams 'integration beams)))
  (('system)
    (ltest-util:rebar-debug "Running system tests ...")
    (let* ((`#(ok ,cwd) (file:get_cwd))
           (beams (ltest:get-system-beams cwd)))
      (ltest-util:rebar-debug "Got cwd: ~p" `(,cwd))
      (ltest-util:rebar-debug "Got beams: ~p" `(,beams))
      (run-beams 'system beams)))
  (('unit)
    (ltest-util:rebar-debug "Running unit tests ...")
    (let* ((`#(ok ,cwd) (file:get_cwd))
           (beams (ltest:get-unit-beams cwd)))
      (ltest-util:rebar-debug "Got cwd: ~p" `(,cwd))
      (ltest-util:rebar-debug "Got beams: ~p" `(,beams))
      (run-beams 'unit beams)))
  ;; Add support for third-party test runners here
  (('selenium)
    (ltest-util:rebar-debug "Running selenium tests ...")
    (case (code:which 'lse)
      ('non_existing (ltest-runner:run-beams 'selenium '()))
      (_ (lse-runner:run-beams))))
  ((beam)
    (run-beam beam (get-listener))))

(defun run-beams (test-type beams)
  (run-beams test-type beams (get-listener)))

(defun run-beams (test-type beams listener)
  (ltest-util:rebar-debug "Running ~p beams using ~p ..."
                          `(,test-type ,listener))
  (eunit:test (ltest-util:beams->files beams)
              (get-options listener `(#(color true)
                                      #(test-type ,test-type)))))

(defun get-listener ()
  "Valid listeners include:
   * ltest-listener
   * eunit_progress
   * eunit_surefire"
   (clj:->> (ltest-util:get-arg 'listener "ltest-listener")
            (element 2)
            (caar)
            (list_to_atom)))

(defun get-options (listener)
  (get-options listener '(colored)))
  ;`(no_tty #(report #(,listener (colored ltest-type)))))

(defun get-options (listener options)
  `(no_tty ,(get-report-options listener options)))

(defun get-report-options (listener options)
  `#(report #(,listener ,options)))

(defun run-beam (beam)
  (run-beam beam (get-listener)))

(defun run-beam (beam listener)
  (run-module (ltest-util:beam->module beam) listener))

(defun run-module (module listener)
  (eunit:test `(,module)
              (get-options listener)))

(defun run-modules (modules)
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  (eunit:test modules (get-options listener)))
