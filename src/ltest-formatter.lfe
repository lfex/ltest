(defmodule ltest-formatter
  "Format EUnit events, summaries, and test names for the ltest report."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun test-suite-header (state)
  "Render the outer report heading using the supplied listener state."
  (io:format (get-suite-header state)))

(defun get-suite-header
  "Return the outer heading as colored iodata according to the selected state."
  (((match-state color? color?))
   (ltest-color:greenb
    (test-header (ltest-const:test-suite-title)
                 (ltest-const:test-suite-header))
    color?)))

(defun test-suite-footer (state)
  "Render the closing report rule using the supplied listener state."
  (io:format (get-suite-footer state)))

(defun get-suite-footer
  "Return the closing rule as colored iodata according to the selected state."
  (((match-state color? color?))
   (io_lib:format "~s~n~n" `(,(ltest-color:greenb
                               (string:copies
                                (ltest-const:test-suite-header)
                                (ltest-const:test-suite-width))
                               color?)))))

(defun test-type-header
  "Render a heading for one unit, system, or integration group."
  ((title (match-state color? color?))
   (io:format
    (ltest-color:blueb
     (test-header title (ltest-const:test-suite-subheader))
     color?))))

(defun test-header (title char)
  "Return a heading padded with the requested rule character."
  (let* ((title (++ " " title " "))
         (width (ltest-const:test-suite-width))
         (header-len (trunc (/ (- width (length title)) 2)))
         (header (++ (string:copies char header-len)
                     title
                     (string:copies char header-len))))
    (io_lib:format "~s~n~n" `(,(string:left
                                 header
                                 width
                                 (car char))))))

(defun indent (count) "Return count spaces for reporter indentation."
  (string:copies " " count))

(defun func-line (raw-func desc)
  "Format a function name and full description with Unicode codepoint-based
   dot padding."
  (let ((func
           (if (=/= 'undefined desc)
               desc
               (get-func-name raw-func))))
    (io:put_chars
      (unicode:characters_to_binary
        (list (indent (ltest-const:func-indent))
              func
              " "
              (get-elision func))))))

(defun get-skip-func-name (func-name)
  "Return a skip function display name without its terminal _skip suffix."
  (let ((name (atom_to_list func-name)))
    (if (lists:suffix "_skip" name)
      (lists:sublist name (- (length name) 5))
      name)))

(defun get-func-name (func-name)
  "Strip a terminal _test or _test_ suffix from an EUnit function name."
  (let ((name (atom_to_list func-name)))
    (cond
      ((lists:suffix "_test_" name)
       (lists:sublist name (- (length name) 6)))
      ((lists:suffix "_test" name)
       (lists:sublist name (- (length name) 5)))
      ('true name))))


(defun get-elision
  "Return zero or more padding dots for the supplied description text;
   never truncate it."
  ((func-name) (when (is_list func-name))
    (let* ((init-len (+ (ltest-const:func-indent)
                        (length func-name)))
           (end-len (length " [fail]"))
           (elide-len (- (ltest-const:test-suite-width)
                         (+ init-len end-len))))
      (string:copies "." (max 0 elide-len))))
  ;; Named tests send description as binary
  ((desc) (when (is_binary desc))
    (get-elision (unicode:characters_to_list desc))))

(defun mod-line-check
  "Render the module-level check line for the supplied event and state."
  (((= (binary (prefix bytes (size 5)) (file bitstring)) desc) (match-state color? color?)) (when (=:= prefix #"file "))
  ;;(((= (binary "file " (mod bitstring)) desc) (match-state color? color?))
   (ltest-util:rebar-debug "Getting mod-line-begin (file) ..." '())
   (ltest-util:rebar-debug "desc: ~p" (list desc))
   (ltest-util:rebar-debug "module (mod-line-begin): ~p" (list (ltest-util:file->module desc)))
   (io_lib:format "~smodule: ~s~n"
                  `(,(indent (ltest-const:mod-indent))
                    ,(ltest-color:greenb
                      (atom_to_list (ltest-util:file->module desc))
                      color?))))
  (((= (binary (prefix bytes (size 7)) (mod bitstring)) desc) (match-state color? color?)) (when (=:= prefix #"module "))
   (ltest-util:rebar-debug "Getting mod-line-begin (module) ..." '())
   (ltest-util:rebar-debug "desc: ~p" (list desc))
   (ltest-util:rebar-debug "module (mod-line-begin): ~p" (list mod))
   (io_lib:format "~smodule: ~s~n"
                  `(,(indent (ltest-const:mod-indent))
                    ,(ltest-color:greenb
                      (ltest-util:binary-mod->str mod)
                      color?))))
  (((= (binary (prefix bytes (size 7)) (rest bitstring)) desc) (match-state color? color?))
   (ltest-util:rebar-debug "Getting mod-line-begin (???) ..." '())
   (ltest-util:rebar-debug "Unexecpted desc: ~p" (list desc))
   (ltest-util:rebar-debug "prefix: ~p" (list prefix))
   (ltest-util:rebar-debug "rest: ~p" (list rest))
   (ltest-util:rebar-debug "Skipping ..." '())
   "")
  ((desc (match-state color? color?))
   (ltest-util:rebar-debug "Getting mod-line-begin (???) ..." '())
   (ltest-util:rebar-debug "Unexecpted desc: ~p" (list desc))
   (ltest-util:rebar-debug "Skipping ..." '())
   ""))

(defun mod-line-begin (desc state)
  "Render the start of a module report section."
  (case (mod-line-check desc state)
    ("" 'skipping)
    (result (io:format result))))

(defun mod-line-end (desc time data state)
  "Render the module summary and elapsed time after its tests finish."
  (ltest-util:rebar-debug "Getting mod-line-end (file) ..." '())
  (ltest-util:rebar-debug "desc: ~p, time: ~p, data: ~p, state: ~p" (list desc time data state))
  (let ((skipped-tests (ltest-util:get-skip-tests desc)))
    (logger:debug "skipped: ~p" (list skipped-tests))
    (ltest-formatter:skip-lines skipped-tests state)
    (ltest-formatter:mod-time time state)))

(defun ok "Render a successful test result."
  (((match-state color? color?))
   (io:format (++ "... [" (ltest-color:greenb "ok" color?) "]~n"))))

(defun fail
  "Display an assertion failure using the configured color setting."
  ((error where (match-state color? color?))
   (io:format ". [~s]~n~n~s~s:~n" `(,(ltest-color:red "fail" color?)
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb
                                     "Assertion failure" color?)))
   (lfe_io:format "~s~s~n~n" `(,(indent (ltest-const:error-indent))
                                ,(ltest-color:redb (io_lib:format "~p" `(,error)) color?)))))

(defun err
  "Display an EUnit error using the configured color setting."
  ((error where (match-state color? color?))
   (io:format " [~s]~n~n~s~s:~n" `(,(ltest-color:yellow "error" color?)
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb "Error" color?)))
   (lfe_io:format "~s~s~n~n" `(,(indent (ltest-const:error-indent))
                                ,(ltest-color:redb (io_lib:format "~p" `(,error)) color?)))))

(defun skip "Render a skipped test result."
  (((match-state color? color?))
   (++ ". [" (ltest-color:blue "skip" color?) "]")))

(defun mod-time "Format elapsed module time for the report."
  ((time (match-state color? color?))
   (io:format "~s~s ~s~s~n~n"
              `(,(indent (ltest-const:func-indent))
                ,(ltest-color:blackb "time:" color?)
                ,(ltest-color:blackb (integer_to_list time) color?)
                ,(ltest-color:blackb "ms" color?)))))

(defun skip-lines (skipped state)
  "Render each skipped test in the listener state."
  (lists:map (lambda (s) (skip-line s state)) skipped))

(defun skip-line "Render one skipped function name with its skip status marker."
  ((`#(,raw-func ,_) state)
    (let ((func (get-skip-func-name raw-func)))
      (io:put_chars
        (unicode:characters_to_binary
          (list (indent (ltest-const:func-indent))
                func
                " "
                (get-elision func)
                (skip state)
                "\n"))))))


(defun display-results (data state)
  "Render the collected result categories in report order."
  (stats-heading state)
  (display-all state)
  (display-successes state)
  (display-skips state)
  (display-failures state)
  (display-errors state)
  ;;(ltest-formatter:display-pending state)
  ;;(ltest-formatter:display-profile state)
  (display-timing state)
  ;;(ltest-formatter:display-results data state)
  (finish-section))

(defun stats-heading "Return the heading for the final result statistics."
  (((match-state color? color?))
   (io:format (++ (ltest-color:yellow "summary:" color?) "~n"))))

(defun display-all (state)
  "Render every result category recorded in the listener state."
  (io:format "~sTests: ~p  " `(,(indent (ltest-const:func-indent))
                               ,(ltest-util:all-tests state))))

(defun display-successes (state)
  "Render successful test results from the listener state."
  (io:format "~s: ~p  " `(,(get-ok-report state)
                          ,(state-ok state))))

(defun display-skips (state)
  "Render skipped test results from the listener state."
  (io:format "~s: ~p  " `(,(get-skip-report state)
                          ,(state-skip state))))

(defun display-failures (state)
  "Render assertion failures from the listener state."
  (io:format "~s: ~p " `(,(get-fail-report state)
                          ,(state-fail state))))

(defun display-errors (state) "Render EUnit errors from the listener state."
  (io:format "~s: ~p~n" `(,(get-err-report state)
                          ,(state-err state))))

(defun display-pending (state)
  "Write the entire listener state with a Pending state label for
   diagnostics."
  (io:format "Pending state: ~p~n" `(,state)))

(defun display-profile (state)
  "Write the entire listener state with a Profile state label for
   diagnostics."
  (io:format "Profile state: ~p~n" `(,state)))

(defun display-timing (state)
  "Render aggregate test timing from the listener state."
  (io:format "~sTotal time: ~pms~n" `(,(indent (ltest-const:func-indent))
                                    ,(state-time state))))

(defun display-no-results
  "Render the no-tests-found message when all test counters are zero."
  ((data (= (match-state color? color?) state))
   (io:format (ltest-color:yellow (get-no-results-report data state) color?))
   (finish-section)))

(defun display-test-cancel (reason) "Render an EUnit cancellation reason."
  (io:format (format-cancel reason)))

(defun format-cancel "Format a cancellation reason as report text."
  (('undefined)  "*skipped*~n")
  (('timeout)    "*timed out*~n")
  ((`#(startup ,reason))
    (io_lib:format "*could not start test process*~n::~tP~n~n"
                   (list reason 15)))
  ((`#(blame ,_subid))
    "*cancelled because of subtask*~n")
  ((`#(exit ,reason))
    (io_lib:format "*unexpected termination of test process*~n::~tP~n~n"
                   (list reason 15)))
  ((`#(abort ,reason))
    (eunit_lib:format_error reason)))

(defun get-no-results-report (data state)
  "Return the no-tests-found report for the supplied data and state."
  (io_lib:format
    "There were no ~s tests found.~n"
    `(,(state-test-type state))))

(defun finish-section () "Return the report separator used to close a section."
  (io:nl)
  (io:nl))

(defun get-ok-report "Return the success report text for the supplied count."
  (((match-state color? color? ok ok))  
   (get-report "Passed" #'ltest-color:greenb/2 ok color?)))

(defun get-skip-report "Return the skip report text for the supplied count."
  (((match-state color? color? skip skip))
   (get-report "Skipped" #'ltest-color:blue/2 skip color?)))

(defun get-fail-report "Return the failure report text for the supplied count."
  (((match-state color? color? fail fail))
   (get-report "Failed" #'ltest-color:red/2 fail color?)))

(defun get-err-report "Return the error report text for the supplied count."
  (((match-state color? color? err err))
   (get-report "Erred" #'ltest-color:yellow/2 err color?)))

(defun get-report (text color-func count color?)
  "Apply a color function to report text when color output is enabled."
  (if (== count 0)
      text
      (funcall color-func text color?)))
