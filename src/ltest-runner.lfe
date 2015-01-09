(defmodule ltest-runner
  (export all))

(defun get-listener ()
  "Valid listeners include:
   * ltest-listener
   * eunit_progress
   * eunit_surefire"
  (list_to_atom
    (caar
      (element 2
        (lutil-file:get-arg
          'listener
          "ltest-listener")))))

(defun get-default-options (listener)
  `(no_tty #(report #(,listener (colored)))))

(defun run
  (('integration)
    (run-beams (ltest:get-integration-beams (lutil-file:get-cwd))))
  (('system)
    (run-beams (ltest:get-system-beams (lutil-file:get-cwd))))
  (('unit)
    (run-beams (ltest:get-unit-beams (lutil-file:get-cwd))))
  ((beam)
    (run-beam beam (get-listener))))

(defun run-module (module listener)
  ; (io:format "Running tests for ~s using listener '~s'~n"
  ;            (list module listener))
  (eunit:test `(,module)
              ; call (get-default-options ...) from above?
              `(no_tty #(report #(,listener (colored))))))

(defun run-modules (modules)
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  (eunit:test modules (get-default-options listener)))

(defun run-beams (beams)
  (run-beams beams (get-listener)))

(defun run-beams (beams listener)
  (eunit:test (lutil-file:beams->files beams)
              (get-default-options listener)))

(defun run-beam (beam)
  (run-beam beam (get-listener)))

(defun run-beam (beam listener)
  (run-module (lutil-file:beam->module beam) listener))

(defun all ()
  (ltest-formatter:test-suite-header)
  (unit 'combined)
  (integration 'combined)
  (system 'combined)
  (ltest-formatter:test-suite-footer))

(defun integration (_)
  (ltest-formatter:test-type-header "Integration Tests")
  (run 'integration))

(defun integration ()
  (ltest-formatter:test-suite-header)
  (integration 'solo)
  (ltest-formatter:test-suite-footer))

(defun system (_)
  (ltest-formatter:test-type-header "System Tests")
  (run 'system))

(defun system ()
  (ltest-formatter:test-suite-header)
  (system 'solo)
  (ltest-formatter:test-suite-footer))

(defun unit (_)
  (ltest-formatter:test-type-header "Unit Tests")
  (run 'unit))

(defun unit ()
  (ltest-formatter:test-suite-header)
  (unit 'solo)
  (ltest-formatter:test-suite-footer))
