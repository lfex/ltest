(defmodule ltest-runner
  (export all))

(defun INT_TEST_HEADER () "Integration Tests")
(defun SYS_TEST_HEADER () "System Tests")
(defun UNIT_TEST_HEADER () "Unit Tests")

(defun all ()
  (ltest-formatter:test-suite-header)
  (unit 'no-suite-headers)
  (integration 'no-suite-headers)
  (system 'no-suite-headers)
  (ltest-formatter:test-suite-footer)
  ;; Add support for third-party test runners here
  (case (code:which 'lse)
    ('non_existing (ltest-runner:run-beams 'selenium '()))
    (_ (lse-runner:selenium 'combined)))
  (ltest-formatter:test-suite-footer))

(defun test-title (test-type)
  (case test-type
   ('integration (INT_TEST_HEADER))
   ('system (SYS_TEST_HEADER))
   ('unit (UNIT_TEST_HEADER))))

(defun run-formatted
  ((test-type 'no-suite-headers)
   (ltest-formatter:test-type-header (test-title test-type))
   (run test-type))
  ((test-type 'suite-headers)
   (ltest-formatter:test-suite-header)
   (run test-type)
   (ltest-formatter:test-suite-footer)))

(defun integration () (integration 'suite-headers))
(defun system () (system 'suite-headers))
(defun unit () (unit 'suite-headers))

(defun integration (flag) (run-formatted 'integration flag))
(defun system (flag) (run-formatted 'system flag))
(defun unit (flag) (run-formatted 'unit flag))

(defun run ()
  (logger:debug "Running with no args ...")
  (run (ltest-const:default-test-type)))

(defun run (test-type)
  (logger:debug "Running for type ~p ..." (list test-type))
  (run (list (ltest-util:app-name)) test-type))

(defun run (apps test-type)
  (logger:debug "Running for apps ~p, type ~p ..." (list apps test-type))
  (run (ltest-const:default-test-profile) apps test-type))

(defun run
  ;; Add ant third-party test runners first:
  ((_ _ 'selenium)
   (logger:debug "Running selenium tests ...")
   (case (code:which 'lse)
     ('non_existing (ltest-runner:run-beams 'selenium '()))
     (_ (lse-runner:run-beams))))
  ;; Lastly, check standard test runners:
  ((profile apps test-type)
   (logger:debug "Running for profile ~p, apps ~p, type ~p ..."
                   (list profile apps test-type))
   (let ((beams (ltest:get-test-beams profile apps test-type)))
     (logger:debug "Got beams: ~~p" (list profile apps test-type beams))
     (run-beams test-type beams))))

(defun run-beams (test-type beams)
  (run-beams test-type beams (get-listener)))

(defun run-beams (test-type beams listener)
  (logger:debug "Running ~p beams using ~p ..."
                          `(,test-type ,listener))
  (eunit:test (ltest-util:beams->files beams)
              (get-options listener `(#(color true)
                                      #(test-type ,test-type)))))

(defun get-listener ()
  (get-listener (ltest-const:default-listener)))

(defun get-listener (listener)
  "Valid listeners include:
   * ltest-listener
   * eunit_progress
   * eunit_surefire"
  (clj:->> (ltest-util:get-arg 'listener listener)
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

(defun run-module (module)
  (run-module module (get-listener)))

(defun run-module (module listener)
  (run-modules `(,module) listener))

(defun run-modules (modules)
  (run-modules modules (get-listener)))

(defun run-modules (modules listener)
  (eunit:test modules (get-options listener)))
