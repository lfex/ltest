(defmodule ltest-runner
  "Provide the legacy test-runner API and dispatch EUnit test groups."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun all ()
  "Run unit, integration, system, then the optional legacy Selenium path;
   return the first non-`ok` result after running each path."
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
  "Run integration tests with the legacy runner and preserve EUnit's
   result. The ignored compatibility argument leaves only the group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Integration Tests" state)
    (run 'integration)))

(defun integration ()
  "Run integration tests with the legacy runner and preserve EUnit's
   result. This zero-argument form adds the outer suite headings around the
   group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (integration 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun system (_)
  "Run system tests with the legacy runner and preserve EUnit's
   result. The ignored compatibility argument leaves only the group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "System Tests" state)
    (run 'system)))

(defun system ()
  "Run system tests with the legacy runner and preserve EUnit's
   result. This zero-argument form adds the outer suite headings around the
   group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (system 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun unit (_)
  "Run unit tests with the legacy runner and preserve EUnit's
   result. The ignored compatibility argument leaves only the group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-type-header "Unit Tests" state)
    (run 'unit)))

(defun unit ()
  "Run unit tests with the legacy runner and preserve EUnit's
   result. This zero-argument form adds the outer suite headings around the
   group heading."
  (let ((state (make-state color? 'true)))
    (ltest-formatter:test-suite-header state)
    (let ((result (unit 'solo)))
      (ltest-formatter:test-suite-footer state)
      result)))

(defun run "Dispatch a supported group or selenium atom; other input is
   treated as a BEAM path. Return the selected EUnit result."
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
  "Append `.beam` to BEAM basenames, then run them through EUnit with the
   listener selected by `get-listener/0`."
  (run-beams test-type beams (get-listener)))

(defun run-beams (test-type beams listener)
  "Run the supplied test BEAM basenames through EUnit with LISTENER."
  (ltest-util:rebar-debug "Running ~p beams using ~p ..."
                          `(,test-type ,listener))
  (eunit:test (ltest-util:beams->files beams)
              (get-options listener `(#(color true)
                                      #(test-type ,test-type)))))

(defun get-listener ()
  "Read the listener init argument, default to `ltest-listener`, and
   convert its first value to an atom."
   (clj:->> (ltest-util:get-arg 'listener "ltest-listener")
            (element 2)
            (caar)
            (list_to_atom)))

(defun get-options (listener)
  "Return `no_tty` and report options for LISTENER with the legacy
   `colored` option; the listener defaults color to true when omitted."
  (get-options listener '(colored)))
  ;`(no_tty #(report #(,listener (colored ltest-type)))))

(defun get-options (listener options)
  "Return `no_tty` plus a report option containing LISTENER and OPTIONS;
   this helper does not validate either value."
  `(no_tty ,(get-report-options listener options)))

(defun get-report-options (listener options)
  "Return EUnit's `report` tuple for LISTENER and OPTIONS."
  `#(report #(,listener ,options)))

(defun run-beam (beam)
  "Read the module identity from the compiled BEAM and run it with the
   listener selected by `get-listener/0`; file and EUnit errors propagate."
  (run-beam beam (get-listener)))

(defun run-beam (beam listener)
  "Resolve the BEAM module and run it through EUnit with LISTENER; the path
   must identify a readable compiled BEAM."
  (run-module (ltest-util:beam->module beam) listener))

(defun run-module (module listener)
  "Run one test module atom through EUnit with LISTENER and return its result."
  (eunit:test `(,module)
              (get-options listener)))

(defun run-modules (modules)
  "Run MODULES through EUnit with the listener selected by `get-listener/0`."
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  "Run one or more test modules through EUnit using LISTENER."
  (eunit:test modules (get-options listener)))
