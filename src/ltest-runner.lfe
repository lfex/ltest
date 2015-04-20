(defmodule ltest-runner
  (export all))

(defun all ()
  (ltest-formatter:test-suite-header)
  (unit 'combined)
  (integration 'combined)
  (system 'combined)
  (ltest-formatter:test-suite-footer)
  (selenium 'combined)
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

(defun selenium (_)
  (ltest-formatter:test-type-header "Selenium Tests")
  (run 'selenium))

(defun selenium ()
  (ltest-formatter:test-suite-header)
  (selenium 'solo)
  (ltest-formatter:test-suite-footer))

(defun unit (_)
  (ltest-formatter:test-type-header "Unit Tests")
  (run 'unit))

(defun unit ()
  (ltest-formatter:test-suite-header)
  (unit 'solo)
  (ltest-formatter:test-suite-footer))

(defun run
  (('integration)
    (run-beams 'integration
               (ltest:get-integration-beams (lutil-file:get-cwd))))
  (('system)
    (run-beams 'system
               (ltest:get-system-beams (lutil-file:get-cwd))))
  (('selenium)
    (run-beams 'selenium
               (ltest-se:get-selenium-beams (lutil-file:get-cwd))))
  (('unit)
    (run-beams 'unit
               (ltest:get-unit-beams (lutil-file:get-cwd))))
  ((beam)
    (run-beam beam (get-listener))))

(defun run-beams (test-type beams)
  (run-beams test-type beams (get-listener)))

(defun run-beams (test-type beams listener)
  (eunit:test (lutil-file:beams->files beams)
              (get-options listener `(#(color true)
                                      #(test-type ,test-type)))))

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
  (run-module (lutil-file:beam->module beam) listener))

(defun run-module (module listener)
  (eunit:test `(,module)
              (get-options listener)))

(defun run-modules (modules)
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  (eunit:test modules (get-options listener)))
