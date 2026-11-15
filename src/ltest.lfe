(defmodule ltest
  "Discover and run unit, system, and integration EUnit tests."
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "ltest/include/ltest-records.lfe")

(defun skip-test-patt ()
  "Return the legacy pattern `.*_skip`; skip detection uses skipped?/1."
  ".*_skip")
(defun skip-test-group-patt ()
  "Return the legacy pattern `(.*)(_skip)` for grouping a skip name."
  "(.*)(_skip)")

;; XXX this code actually assumes two possible layouts for different versions of
;;     rebar; those paths don't belong in a general purpose library, but rather
;;     in a rebar3 plugin.
;;
;;     instead, there should be two path functions:
;;     - one that takes two arguments, a prefix and a regex, where the two are
;;       filename:joined and then pumped into filelib:wildcard, and
;;     - one that takes one argument, a regex, and then calls the two-arg
;;       function with a default (e.g., ".")
;;
;;     come to think of it, though, that also feels like a bad approach. it
;;     should just recurse through a given directory, and look for beam files.
;;     in a rebar3 plugin, the appropriate path can be computed.
;;
;;     so, some examination should be done of the latest rebar3 code, to see
;;     what public utility functions there are for locating beam files for a
;;     given app (or, in the case of releases, set of apps). That would mean
;;     we wouldn't need to do any wild-carding whatsoever ...
;;
;;     see XXX comment on `get-possible-test-beams` for more thoughts ...
;;
;;     related ticket: https://github.com/lfex/ltest/issues/61
;;
(defun get-subdir-beam-files (path)
  "Return BEAM paths found under PATH in `.eunit/` and `_build/*/lib/*/ebin/`.
   Results follow the wildcard order for each directory pattern."
  (ltest-util:rebar-debug "Getting subdir beams ..." '())
  (lists:append
    (list
      (filelib:wildcard (filename:join
        (list path ".eunit/*.beam")))
      (filelib:wildcard (filename:join
        (list path "_build/*/lib/*/ebin/*.beam"))))))

;; XXX do we need this? after examining the current rebar3 utils, we might not.
;;
;;     this library baiscally uses behaviours as "marker interfaces" and that's
;;     why we're jumping through all these hoops. so we should ask some basic
;;     questions:
;;
;;     - is there a better way to do this in Erlang/LFE? for example, eunit
;;       expects functions to have a particular suffix; we could do the same
;;       to distinguish unit, system, and integration tests
;;     - is there a more elegant way to check for a behaviour in a beam file?
;;     - does rebar3 offer some utility functions that do this better, that we
;;       can use?
;;
;;     related ticket: https://github.com/lfex/ltest/issues/61
;;
(defun get-possible-test-beams (path)
  "Return discovered BEAM path roots without `.beam`, suitable for
   `code:load_abs/1`."
  (ltest-util:rebar-debug "Getting possible test beams ..." '())
  (lists:map
    #'filename:rootname/1
    (get-subdir-beam-files path)))

(defun get-integration-beams ()
  "Find integration-test BEAM basenames beneath the current project directory."
  (get-integration-beams "."))

(defun get-integration-beams (path)
  "Return BEAM path roots under PATH whose files declare `ltest-integration`.
   PATH must contain the supported BEAM search directories."
  (lists:filter
    #'integration?/1
    (get-possible-test-beams path)))

(defun get-system-beams ()
  "Find system-test BEAM basenames beneath the current project directory."
  (get-system-beams "."))

(defun get-system-beams (path)
  "Return BEAM path roots under PATH whose files declare `ltest-system`.
   PATH must contain the supported BEAM search directories."
  (ltest-util:rebar-debug "Getting system beams from ~p ..." `(,path))
  (lists:filter
    #'system?/1
    (get-possible-test-beams path)))

(defun get-unit-beams ()
  "Find unit-test BEAM basenames beneath the current project directory."
  (get-unit-beams "."))

(defun get-unit-beams (path)
  "Return BEAM path roots under PATH whose files declare `ltest-unit`.
   PATH must contain the supported BEAM search directories."
  (lists:filter
    #'unit?/1
    (get-possible-test-beams path)))

(defun has-behaviour? (beam type)
  "Return whether a readable BEAM file lists TYPE in its behavior attributes."
  (ltest-util:rebar-debug "Checking if ~p has behaviour ~p ..." `(,beam ,type))
  (lists:member
    type
    (ltest-util:get-beam-behaviours beam)))

(defun integration? (beam)
  "Return whether a BEAM file declares ltest-integration."
  (has-behaviour? beam 'ltest-integration))

(defun system? (beam) "Return whether a BEAM file declares ltest-system."
  (has-behaviour? beam 'ltest-system))

(defun unit? (beam) "Return whether a BEAM file declares ltest-unit."
  (has-behaviour? beam 'ltest-unit))

(defun check-skip-funcs (funcs)
  "Map exported name/arity pairs to themselves when they are nullary `_skip`
   functions; map other entries to `false`."
  (lists:map
    (match-lambda
      (((tuple func arity))
        (if (ltest-util:skipped? (tuple func arity))
          `#(,func ,arity)
          'false)))
    funcs))

(defun check-skipped-tests (funcs)
  "Map exported nullary `_skip` functions to their name without that suffix;
   return the name as a character list and map other entries to `false`."
  (lists:map
    (match-lambda
      (((tuple func arity))
        (if (ltest-util:skipped? (tuple func arity))
          (lists:sublist (atom_to_list func)
                         (- (length (atom_to_list func)) 5))
          'false)))
    funcs))

(defun get-skip-funcs (module)
  "Return nullary `_skip` exports from a module whose BEAM resolves on the
   code path, filtering other exports."
  (ltest-util:filtered
    #'check-skip-funcs/1
    (ltest-util:get-module-exports module)))

(defun get-skipped-tests (module)
  "Return character-list names of nullary `_skip` exports from a module whose
   BEAM resolves on the code path, filtering other exports."
  (ltest-util:filtered
    #'check-skipped-tests/1
    (ltest-util:get-module-exports module)))

(defun check-failed-assert (data expected)
  "Unwrap an assertion failure and assert that its embedded failure tag
   matches expected."
  (let ((`#(,failure-type ,_) data))
    (is-equal failure-type expected)))

(defun check-wrong-assert-exception (data expected)
  "Unwrap an unexpected assertion exception and assert that its failure tag
   matches expected."
  (let* ((reason (assert-exception-failed))
         (`#(,reason (,_ ,_ ,_ ,_ #(,fail-type ,_))) data))
    (is-equal fail-type expected)))

;;; New API for running tests
;;;
;;; Options are merged with default-opts at every public group boundary. Group
;;; functions preserve EUnit's result; run/0,1 adapts that result for rebar3.

(defun run ()
  "Run the default unit group, then map EUnit `error` to
   `{error, tests_failed}` and preserve other results."
  (run (default-opts)))

(defun run (opts)
  "Merge OPTS over the defaults, dispatch the selected group, and map EUnit
   `error` to `{error, tests_failed}`. Preserve other results; unknown types
   return an explanatory `{error, Message}`."
  (let* ((merged-opts (maps:merge (default-opts) opts))
         (result
           (case (mref merged-opts 'test-type)
             ('all (all merged-opts))
             ('unit (unit merged-opts))
             ('system (system merged-opts))
             ('integration (integration merged-opts))
             (test-type `#(error ,(io_lib:format "Unknown test type: ~p" `(,test-type)))))))
    (adapt-run-result result)))

(defun adapt-run-result (result)
  "Map EUnit error to {error, tests_failed}; preserve ok and other error
   tuples."
  (case result
    ('ok 'ok)
    ('error `#(error tests_failed))
    (`#(error ,_) result)))

(defun all ()
  "Run unit, system, then integration in order, even when an earlier group
   fails, and return the first non-`ok` result."
  (all (default-opts)))

(defun all (opts)
  "Run all groups in unit, system, integration order, even after failure.
   Optional `modules`, `module`, or `suite` names select discovered modules;
   invalid requests return a tagged error before any test executes."
  (let* ((merged-opts (merge-run-opts opts 'all))
         (test-opts (maps:merge merged-opts #m(suite-headers false)))
         (state (make-state color? (mref merged-opts 'color)))
         (_header (maybe-suite-header merged-opts state))
         (result (case (normalize-selectors merged-opts)
                   (`#(error ,reason) `#(error ,reason))
                   (`#(ok unfiltered)
                     (first-non-ok
                       (list
                         (unit (maps:merge test-opts #m(test-type unit)))
                         (system (maps:merge test-opts #m(test-type system)))
                         (integration
                           (maps:merge test-opts #m(test-type integration))))))
                   (`#(ok ,names)
                     (run-all-selected names test-opts))))
         (_footer (maybe-suite-footer merged-opts state)))
    result))

(defun unit ()
  "Run the unit group with complete default options and return EUnit's
   canonical result."
  (unit (default-opts)))

(defun unit (opts)
  "Merge OPTS over defaults, force `test-type` to `unit`, and return the
   canonical EUnit result."
  (run-group 'unit "Unit Tests" opts #'get-unit-beams/1))

(defun system ()
  "Run the system group with complete default options and return EUnit's
   canonical result."
  (system (default-opts)))

(defun system (opts)
  "Merge OPTS over defaults, force `test-type` to `system`, and return the
   canonical EUnit result."
  (run-group 'system "System Tests" opts #'get-system-beams/1))

(defun integration ()
  "Run the integration group with complete default options and return
   EUnit's canonical result."
  (integration (default-opts)))

(defun integration (opts)
  "Merge OPTS over defaults, force `test-type` to `integration`, and return
   the canonical EUnit result."
  (run-group 'integration "Integration Tests" opts #'get-integration-beams/1))

(defun run-group (test-type title opts test-fn)
  "Run TEST-TYPE with merged defaults, always write its group heading, and
   write the outer suite headings when enabled; return EUnit's result."
  (let* ((merged-opts (merge-run-opts opts test-type))
         (state (make-state color? (mref merged-opts 'color)
                            test-type test-type)))
    (maybe-suite-header merged-opts state)
    (ltest-formatter:test-type-header title state)
    (let ((result (run-beams test-type test-fn merged-opts)))
      (maybe-suite-footer merged-opts state)
      result)))

(defun run-all-selected (names opts)
  "Discover all groups, validate NAMES against their union, then run each
   prepared group in canonical order without repeating selector validation."
  (let* ((`#(ok ,cwd) (file:get_cwd))
         (groups (list (get-unit-beams cwd)
                       (get-system-beams cwd)
                       (get-integration-beams cwd))))
    (case (select-discovered names groups)
      (`#(error ,reason) `#(error ,reason))
      (`#(ok ,selected)
        (let ((results
                (list
                  (run-prepared-group
                    'unit "Unit Tests" (lists:nth 1 selected) opts)
                  (run-prepared-group
                    'system "System Tests" (lists:nth 2 selected) opts)
                  (run-prepared-group 'integration "Integration Tests"
                                      (lists:nth 3 selected) opts))))
          (first-non-ok results))))))

(defun run-prepared-group (test-type title beams opts)
  "Write the group heading and execute BEAMS already validated for the
   complete selection request."
  (let ((state (make-state color? (mref opts 'color)
                           test-type test-type)))
    (ltest-formatter:test-type-header title state)
    (execute-beams test-type beams opts)))

(defun first-non-ok
  "Return the first non-ok result, or ok when every group succeeded."
  ((()) 'ok)
  (((cons 'ok rest)) (first-non-ok rest))
  (((cons result _rest)) result))

(defun merge-run-opts (opts test-type)
  "Merge the defaults with OPTS, then replace `test-type` with TEST-TYPE."
  (maps:put 'test-type test-type (maps:merge (default-opts) opts)))

(defun default-opts ()
  "Return defaults: color and suite headers enabled, listener `ltest-listener`,
   test type `unit`, and verbose output enabled."
  #m(color true
     suite-headers true
     test-listener ltest-listener
     test-type unit
     verbose true))

(defun maybe-suite-header (opts state)
  "Write the outer suite heading when OPTS has Boolean `true` for
   `suite-headers`."
  (if (mref opts 'suite-headers)
    (ltest-formatter:test-suite-header state)))

(defun maybe-suite-footer (opts state)
  "Write the outer suite footer when OPTS has Boolean `true` for
   `suite-headers`."
  (if (mref opts 'suite-headers)
    (ltest-formatter:test-suite-footer state)))

(defun eunit-opts (opts)
  "Build EUnit options from the required `test-listener`, `verbose`,
   `color`, and `test-type` keys. `eunit_surefire` selects tty output;
   other listeners use `no_tty`."
  (let* ((listener (mref opts 'test-listener))
         (tty (case listener
                ('eunit_surefire '())
                (_ '(no_tty))))
         (verbose (if (and (== listener 'eunit_surefire)
                           (mref opts 'verbose))
                    '(verbose)
                    '()))
         (report `(#(report #(,listener ,(ltest-opts opts))))))
    (lists:append `(,tty ,verbose ,report))))

(defun ltest-opts (opts)
  "Return listener option tuples for the required `color` and `test-type`
   keys; this helper does not merge defaults."
  `(#(color ,(mref opts 'color))
    #(test-type ,(mref opts 'test-type))))

(defun run-beams (test-type test-fn opts)
  "Discover TEST-TYPE beams, validate selector options against their declared
   modules, and pass selected files and translated options to EUnit."
  (let* ((merged-opts (merge-run-opts opts test-type)))
    (case (normalize-selectors merged-opts)
      (`#(error ,reason) `#(error ,reason))
      (`#(ok ,selection)
        (let* ((`#(ok ,cwd) (file:get_cwd))
               (beams (funcall test-fn cwd)))
          (case (select-discovered selection (list beams))
            (`#(error ,reason) `#(error ,reason))
            (`#(ok (,selected))
              (execute-beams test-type selected merged-opts))))))))

(defun execute-beams (test-type beams opts)
  "Pass already selected TEST-TYPE BEAM roots and merged OPTS to EUnit."
  (eunit:test (ltest-util:beams->files beams)
              (eunit-opts (merge-run-opts opts test-type))))

(defun normalize-selectors (opts)
  "Validate the unsupported `test` option and normalize the three module
   selector aliases; return `unfiltered` when none is supplied."
  (case (maps:find 'test opts)
    ('error (normalize-selector-aliases opts))
    (`#(ok undefined) (normalize-selector-aliases opts))
    (`#(ok ,value) `#(error #(unsupported_test_selector ,value)))))

(defun normalize-selector-aliases (opts)
  "Normalize aliases in fixed `modules`, `module`, `suite` order and require
   their sorted binary name sets to agree."
  (case (normalize-selector-aliases '(modules module suite) opts '())
    (`#(error ,reason) `#(error ,reason))
    (`#(ok ()) `#(ok unfiltered))
    (`#(ok ,aliases)
      (let* ((`#(,_ ,names) (car aliases)))
        (case (selector-aliases-agree? names aliases)
          ('true `#(ok ,names))
          ('false `#(error #(conflicting_selectors ,aliases))))))))

(defun normalize-selector-aliases (keys opts acc)
  "Normalize present selector aliases in KEYS order, preserving each key and
   original raw value for deterministic errors."
  (case keys
    ('() `#(ok ,(lists:reverse acc)))
    (`(,key . ,rest)
      (case (maps:find key opts)
        ('error (normalize-selector-aliases rest opts acc))
        (`#(ok undefined) (normalize-selector-aliases rest opts acc))
        (`#(ok ,raw)
          (case (normalize-selector-value raw)
            (`#(ok ,names)
              (normalize-selector-aliases rest opts
                                          (cons `#(,key ,names) acc)))
            (`#(error ,_)
              `#(error #(invalid_selector ,key ,raw)))))))))

(defun selector-aliases-agree? (names aliases)
  "Return whether each normalized alias contains exactly NAMES."
  (case aliases
    ('() 'true)
    (`(#(,_ ,alias-names) . ,rest)
      (andalso (=:= names alias-names)
               (selector-aliases-agree? names rest)))))

(defun normalize-selector-value
  "Convert one selector scalar or nonempty list of selector scalars to sorted
   distinct UTF-8 binary names; reject empty and unsupported values."
  ((value) (when (is_atom value))
    (normalize-selector-text (atom_to_binary value 'utf8)))
  ((value) (when (is_binary value))
    (normalize-selector-text value))
  ((()) `#(error invalid))
  ((value) (when (is_list value))
    (case (proper-selector-list? value)
      ('false `#(error invalid))
      ('true
        (case (selector-charlist? value)
          ('true (normalize-selector-text (unicode:characters_to_binary value)))
          ('false (normalize-selector-items value '()))))))
  ((_) `#(error invalid)))

(defun proper-selector-list? (value)
  "Return whether VALUE is a proper list, including nested selector-list
   values, without assuming `is_list/1` excludes an improper tail."
  (case value
    ('() 'true)
    (`(,_ . ,tail) (proper-selector-list? tail))
    (_ 'false)))

(defun selector-charlist? (value)
  "Return whether nonempty VALUE is a proper list containing only integers."
  (case value
    ('() 'true)
    (`(,head . ,tail)
      (andalso (is_integer head) (selector-charlist? tail)))
    (_ 'false)))

(defun normalize-selector-items (items acc)
  "Normalize each scalar item and combine its names, rejecting any non-scalar
   member while retaining no input order."
  (case items
    ('() `#(ok ,(lists:usort acc)))
    (`(,item . ,rest)
      (case (normalize-selector-scalar item)
        (`#(ok ,names) (normalize-selector-items rest (lists:append names acc)))
        (`#(error ,_) `#(error invalid))))))

(defun normalize-selector-scalar
  "Normalize one atom, UTF-8 binary, or Unicode character-list selector."
  ((value) (when (is_atom value))
    (normalize-selector-text (atom_to_binary value 'utf8)))
  ((value) (when (is_binary value))
    (normalize-selector-text value))
  ((value) (when (is_list value))
    (case (selector-charlist? value)
      ('true (normalize-selector-text (unicode:characters_to_binary value)))
      ('false `#(error invalid))))
  ((_) `#(error invalid)))

(defun normalize-selector-text (text)
  "Validate UTF-8 TEXT, split ASCII commas, trim ASCII edge whitespace, and
   return sorted distinct nonempty binary names."
  (normalize-selector-text-result (unicode:characters_to_binary text)))

(defun normalize-selector-text-result
  "Normalize the valid UTF-8 binary returned by the Unicode conversion, or
   reject its tagged invalid/incomplete result."
  ((text-bin) (when (is_binary text-bin))
    (normalize-selector-tokens
      (binary:split text-bin #"," '(global)) '()))
  ((_) `#(error invalid)))

(defun normalize-selector-tokens (tokens acc)
  "Trim selector TOKENS and return their sorted distinct names, rejecting an
   empty token."
  (case tokens
    ('() `#(ok ,(lists:usort acc)))
    (`(,token . ,rest)
      (let ((trimmed (trim-selector-space token)))
        (case (byte_size trimmed)
          (0 `#(error invalid))
          (_ (normalize-selector-tokens rest (cons trimmed acc))))))))

(defun trim-selector-space (text)
  "Remove only ASCII space, tab, carriage return, and newline at both edges."
  (trim-selector-right (trim-selector-left text)))

(defun trim-selector-left (text)
  "Remove leading ASCII selector whitespace from TEXT."
  (case (byte_size text)
    (0 text)
    (_ (case (selector-space? (binary:at text 0))
         ('true (trim-selector-left
                  (binary:part text 1 (- (byte_size text) 1))))
         ('false text)))))

(defun trim-selector-right (text)
  "Remove trailing ASCII selector whitespace from TEXT."
  (case (byte_size text)
    (0 text)
    (_ (case (selector-space? (binary:last text))
         ('true (trim-selector-right
                  (binary:part text 0 (- (byte_size text) 1))))
         ('false text)))))

(defun selector-space? (char)
  "Return whether CHAR is one of the four ASCII selector whitespace bytes."
  (orelse (=:= char 32)
          (=:= char 9)
          (=:= char 13)
          (=:= char 10)))

(defun select-discovered
  "Validate selection against the union of discovered GROUPS and return each
   selected BEAM list in its original discovery order."
  (('unfiltered groups) `#(ok ,groups))
  ((names groups)
    (let* ((all-beams (lists:append groups))
           (available (lists:usort
                        (lists:map #'discovered-module-name/1 all-beams)))
           (missing (lists:subtract names available)))
      (case missing
        ('() `#(ok ,(select-beam-groups names groups)))
        (_ `#(error #(unmatched_modules ,missing)))))))

(defun select-beam-groups (names groups)
  "Filter each discovered group by NAMES while preserving group and BEAM
   ordering and duplicate discovery paths."
  (lists:map (lambda (beams) (select-discovered-beams names beams)) groups))

(defun select-discovered-beams (names beams)
  "Keep discovered BEAM roots whose declared module identity appears in NAMES."
  (lists:filter
    (lambda (beam) (lists:member (discovered-module-name beam) names))
    beams))

(defun discovered-module-name (beam)
  "Return the UTF-8 binary identity declared in a discovered BEAM file."
  (atom_to_binary (ltest-util:beam->module beam) 'utf8))
