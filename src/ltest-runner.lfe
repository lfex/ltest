(defmodule ltest-runner
  "Provide the legacy test-runner API and dispatch EUnit test groups."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun all ()
  "Run unit, integration, system, and available legacy extensions; return
   the first non-ok result."
  (let* ((state (make-state color? 'true))
         (_header (ltest-formatter:test-suite-header state))
         (results (list
                    (unit 'combined)
                    (integration 'combined)
                    (system 'combined)
                    ;; Add support for third-party test runners here
                    (case (code:which 'lse)
                      ('non_existing (ltest-runner:run-beams 'selenium '()))
                      (_ (lse-runner:selenium 'combined)))))
         (_footer (ltest-formatter:test-suite-footer state)))
    (first-non-ok results)))

(defun first-non-ok
  "Return the first non-ok legacy group result, or ok on full success."
  ((()) 'ok)
  (((cons 'ok rest)) (first-non-ok rest))
  (((cons result _rest)) result))

(defun integration (_)
  "Run integration tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Integration Tests" state)
    (run 'integration)))

(defun integration ()
  "Run integration tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (integration 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun system (_)
  "Run system tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "System Tests" state)
    (run 'system)))

(defun system ()
  "Run system tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (system 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun unit (_)
  "Run unit tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Unit Tests" state)
    (run 'unit)))

(defun unit ()
  "Run unit tests; the options argument is accepted for legacy API
   compatibility."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (unit 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun run "Dispatch the configured legacy test type and report options."
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
  "Run the supplied test BEAM basenames through EUnit with the selected
   listener."
  (run-beams test-type beams (get-listener)))

(defun run-beams (test-type beams listener)
  "Run the supplied test BEAM basenames through EUnit with the selected
   listener."
  (ltest-util:rebar-debug "Running ~p beams using ~p ..."
                          `(,test-type ,listener))
  (eunit:test (ltest-util:beams->files beams)
              (get-options listener `(#(color true)
                                      #(test-type ,test-type)))))

(defun get-listener ()
  "Return the listener selected by the init argument; default to
   ltest-listener."
   (clj:->> (ltest-util:get-arg 'listener "ltest-listener")
            (element 2)
            (caar)
            (list_to_atom)))

(defun get-options (listener)
  "Return no_tty plus report options for LISTENER with colored output."
  (get-options listener '(colored)))
  ;`(no_tty #(report #(,listener (colored ltest-type)))))

(defun get-options (listener options)
  "Return no_tty plus report options for LISTENER using OPTIONS."
  `(no_tty ,(get-report-options listener options)))

(defun get-report-options (listener options)
  "Wrap LISTENER and OPTIONS in EUnit's report option tuple."
  `#(report #(,listener ,options)))

(defun run-beam (beam)
  "Run one compiled test BEAM through EUnit with the selected listener."
  (run-beam beam (get-listener)))

(defun run-beam (beam listener)
  "Run one compiled test BEAM through EUnit with the selected listener."
  (run-module (ltest-util:beam->module beam) listener))

(defun run-module (module listener)
  "Run one test module through EUnit with the selected listener."
  (eunit:test `(,module)
              (get-options listener)))

(defun run-modules (modules)
  "Run one or more test modules through EUnit using the selected listener."
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  "Run one or more test modules through EUnit using the selected listener."
  (eunit:test modules (get-options listener)))
