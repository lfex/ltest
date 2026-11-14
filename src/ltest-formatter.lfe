(defmodule ltest-formatter
  "Format EUnit events and write or return the text and iodata used by the
   ltest report."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun test-suite-header (state)
  "Write the outer report heading to the current IO device and return the
   `io:format/1` result."
  (io:format (get-suite-header state)))

(defun get-suite-header
  "Return the outer heading as colored iodata without writing it."
  (((match-state color? color?))
   (ltest-color:greenb
    (test-header (ltest-const:test-suite-title)
                 (ltest-const:test-suite-header))
    color?)))

(defun test-suite-footer (state)
  "Write the closing rule and two trailing newlines to the current IO
   device; return the `io:format/1` result."
  (io:format (get-suite-footer state)))

(defun get-suite-footer
  "Return the colored closing rule and two newlines as iodata without
   writing it."
  (((match-state color? color?))
   (io_lib:format "~s~n~n" `(,(ltest-color:greenb
                               (string:copies
                                (ltest-const:test-suite-header)
                                (ltest-const:test-suite-width))
                               color?)))))

(defun test-type-header
  "Write the colored group heading to the current IO device and return the
   `io:format/1` result."
  ((title (match-state color? color?))
   (io:format
    (ltest-color:blueb
     (test-header title (ltest-const:test-suite-subheader))
     color?))))

(defun test-header (title char)
  "Return centered heading iodata. TITLE is a character list whose
   surrounding spaces fit the configured width; CHAR is a one-character
   string used for the rule and as the fill character when needed."
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

(defun indent (count) "Return a string containing COUNT spaces."
  (string:copies " " count))

(defun func-line (raw-func desc)
  "Write one test line using DESC unless it is `undefined`, in which case use
   the suffix-stripped name from atom RAW-FUNC. Append zero-clamped codepoint
   padding and return the IO result. DESC may be flat Unicode text or a valid
   Unicode binary."
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
  "Return FUNC-NAME without a terminal `_skip`, or its unchanged atom name,
   as a character list."
  (let ((name (atom_to_list func-name)))
    (if (lists:suffix "_skip" name)
      (lists:sublist name (- (length name) 5))
      name)))

(defun get-func-name (func-name)
  "Return an EUnit function name without terminal `_test` or `_test_`, or
   preserve other atom-name text, as a character list."
  (let ((name (atom_to_list func-name)))
    (cond
      ((lists:suffix "_test_" name)
       (lists:sublist name (- (length name) 6)))
      ((lists:suffix "_test" name)
       (lists:sublist name (- (length name) 5)))
      ('true name))))


(defun get-elision
  "Return dots for the available report width after FUNC-INDENT and the
   failure marker; clamp the count at zero and never truncate text. Accept a
   flat Unicode list or valid Unicode binary."
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
  "Return module-line iodata for a recognized descriptor without writing.
   A `file ` descriptor must be the quoted EUnit form for a readable BEAM;
   descriptor and BEAM errors propagate. Other prefixes return an empty
   string."
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
  "Write a recognized module line and return the IO result; return
   `skipping` when MOD-LINE-CHECK returns an empty string."
  (case (mod-line-check desc state)
    ("" 'skipping)
    (result (io:format result))))

(defun mod-line-end (desc time data state)
  "Write manual skip rows and module elapsed time. DATA is included in the
   debug log but does not determine these visible rows; no group category
   summary is printed."
  (ltest-util:rebar-debug "Getting mod-line-end (file) ..." '())
  (ltest-util:rebar-debug "desc: ~p, time: ~p, data: ~p, state: ~p" (list desc time data state))
  (let ((skipped-tests (ltest-util:get-skip-tests desc)))
    (logger:debug "skipped: ~p" (list skipped-tests))
    (ltest-formatter:skip-lines skipped-tests state)
    (ltest-formatter:mod-time time state)))

(defun ok "Write the successful-test marker to the current IO device and
   return the `io:format/1` result."
  (((match-state color? color?))
   (io:format (++ "... [" (ltest-color:greenb "ok" color?) "]~n"))))

(defun fail
  "Write the assertion-failure label and ERROR term using STATE color; WHERE
   is unused. Return the final formatting IO result."
  ((error where (match-state color? color?))
   (io:format ". [~s]~n~n~s~s:~n" `(,(ltest-color:red "fail" color?)
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb
                                     "Assertion failure" color?)))
   (lfe_io:format "~s~s~n~n" `(,(indent (ltest-const:error-indent))
                                ,(ltest-color:redb (io_lib:format "~p" `(,error)) color?)))))

(defun err
  "Write the EUnit error label and ERROR term using STATE color; WHERE is
   unused. Return the final formatting IO result."
  ((error where (match-state color? color?))
   (io:format " [~s]~n~n~s~s:~n" `(,(ltest-color:yellow "error" color?)
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb "Error" color?)))
   (lfe_io:format "~s~s~n~n" `(,(indent (ltest-const:error-indent))
                                ,(ltest-color:redb (io_lib:format "~p" `(,error)) color?)))))

(defun skip "Return the skipped-test marker iodata using STATE color;
   perform no IO."
  (((match-state color? color?))
   (++ ". [" (ltest-color:blue "skip" color?) "]")))

(defun mod-time "Write TIME in milliseconds using STATE color; return the
   `io:format/2` result."
  ((time (match-state color? color?))
   (io:format "~s~s ~s~s~n~n"
              `(,(indent (ltest-const:func-indent))
                ,(ltest-color:blackb "time:" color?)
                ,(ltest-color:blackb (integer_to_list time) color?)
                ,(ltest-color:blackb "ms" color?)))))

(defun skip-lines (skipped state)
  "Write one line for each skipped entry and return the list of each
   `skip-line/2` IO result."
  (lists:map (lambda (s) (skip-line s state)) skipped))

(defun skip-line "Write one skipped-test line for `{Function, Arity}` and
   return the `io:put_chars/1` result."
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
  "Write the summary heading, total and category counts, timing, and final
   blank lines. DATA is unused; return the final IO result."
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

(defun stats-heading "Write `summary:` and a newline to the current IO device;
   return the `io:format/1` result."
  (((match-state color? color?))
   (io:format (++ (ltest-color:yellow "summary:" color?) "~n"))))

(defun display-all (state)
  "Write only the aggregate Tests count from STATE, including cancellations;
   return the `io:format/2` result."
  (io:format "~sTests: ~p  " `(,(indent (ltest-const:func-indent))
                               ,(ltest-util:all-tests state))))

(defun display-successes (state)
  "Write the Passed label and STATE's success count; return the IO result."
  (io:format "~s: ~p  " `(,(get-ok-report state)
                          ,(state-ok state))))

(defun display-skips (state)
  "Write the Skipped label and STATE's skip count; return the IO result."
  (io:format "~s: ~p  " `(,(get-skip-report state)
                          ,(state-skip state))))

(defun display-failures (state)
  "Write the Failed label and STATE's assertion-failure count; return the IO
   result."
  (io:format "~s: ~p " `(,(get-fail-report state)
                          ,(state-fail state))))

(defun display-errors (state) "Write the Erred label and STATE's EUnit error
   count; return the `io:format/2` result."
  (io:format "~s: ~p~n" `(,(get-err-report state)
                          ,(state-err state))))

(defun display-pending (state)
  "Write STATE with the diagnostic label `Pending`; return the IO result."
  (io:format "Pending state: ~p~n" `(,state)))

(defun display-profile (state)
  "Write STATE with the diagnostic label `Profile`; return the IO result."
  (io:format "Profile state: ~p~n" `(,state)))

(defun display-timing (state)
  "Write STATE's total elapsed time, formatted as milliseconds; return the
   IO result."
  (io:format "~sTotal time: ~pms~n" `(,(indent (ltest-const:func-indent))
                                    ,(state-time state))))

(defun display-no-results
  "Write the no-tests message using STATE's test type, then write the final
   blank lines. This function does not inspect counters; DATA is unused."
  ((data (= (match-state color? color?) state))
   (io:format (ltest-color:yellow (get-no-results-report data state) color?))
   (finish-section)))

(defun display-test-cancel (reason) "Write `format-cancel/1` reason text and
   return the IO result."
  (io:format (format-cancel reason)))

(defun format-cancel "Return branch-specific EUnit cancellation text or
   formatter output; some returned iodata contains format directives for a
   later formatter."
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
  "Return a formatted no-tests message using STATE's test type; DATA is
   unused and no test-counter check is performed."
  (io_lib:format
    "There were no ~s tests found.~n"
    `(,(state-test-type state))))

(defun finish-section () "Write two newline characters to the current IO
   device and return the second `io:nl/0` result."
  (io:nl)
  (io:nl))

(defun get-ok-report "Return `Passed` at zero; otherwise call the green bold
   color helper with STATE's color flag."
  (((match-state color? color? ok ok))  
   (get-report "Passed" #'ltest-color:greenb/2 ok color?)))

(defun get-skip-report "Return `Skipped` at zero; otherwise call the blue
   color helper with STATE's color flag."
  (((match-state color? color? skip skip))
   (get-report "Skipped" #'ltest-color:blue/2 skip color?)))

(defun get-fail-report "Return `Failed` at zero; otherwise call the red
   color helper with STATE's color flag."
  (((match-state color? color? fail fail))
   (get-report "Failed" #'ltest-color:red/2 fail color?)))

(defun get-err-report "Return `Erred` at zero; otherwise call the yellow
   color helper with STATE's color flag."
  (((match-state color? color? err err))
   (get-report "Erred" #'ltest-color:yellow/2 err color?)))

(defun get-report (text color-func count color?)
  "Return TEXT when COUNT is zero; otherwise call COLOR-FUNC(TEXT, COLOR?)
   even when COLOR? is false."
  (if (== count 0)
      text
      (funcall color-func text color?)))
