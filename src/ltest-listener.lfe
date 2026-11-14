(defmodule ltest-listener
  "Collect EUnit events and render the ltest report."
  (behaviour eunit_listener)
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun start () "Call `eunit_listener:start/2` with this listener and no
   options; return its result."
  (start '()))

(defun start (options)
  "Forward OPTIONS to `eunit_listener:start/2` and return its result; spawn
   options can affect that result."
  (eunit_listener:start (MODULE) options))

(defun init (options)
  "Create listener state with empty per-module status and zeroed counters;
   use `color=true` and `test-type=unit` when those options are absent."
  (make-state
   status (orddict:new)
   test-type (proplists:get_value 'test-type options 'unit)
   color?  (proplists:get_value 'color options 'true)))

(defun handle_begin "Handle an EUnit begin event, writing recognized group
   and test headings or lines; return the unchanged listener state."
  (('group (= `(,_ #(desc undefined) ,_ ,_) data) state)
    (ltest-util:rebar-debug "data (group, no desc): ~p" (list data))
    state)
  (('group (= `(,_ #(desc ,desc) ,_ ,_) data) state)
    (ltest-util:rebar-debug "desc, data: ~p, ~p" (list desc data))
    (case (binary:match desc (binary "file"))
      ('nomatch 'skipping)
      (_ (ltest-formatter:mod-line-begin desc state)))
    state)
  (('test (= `(,_ ,_ #(source #(,mod ,func ,_arity)) ,_) data) state)
    (ltest-util:rebar-debug "test match m/f ~p/~p" (list mod func))
    (ltest-util:rebar-debug "data: ~p" (list data))
    (ltest-formatter:func-line func
                               (proplists:get_value 'desc data 'undefined))
    state)
  (('test data state)
    (io:format "\tstarting test ...~n" '())
    (ltest-util:rebar-debug "data: ~p~n" (list data))
    state)
  ((type data state)
   (ltest-util:rebar-debug "*** UNEXPECTED ***" '())
   (ltest-util:rebar-debug "type: ~p~ndata: ~p~n: state: ~p" (list type data state))
   state))

(defun handle_end
  "Handle an EUnit end event: update result counters or account for each
   module's skips and elapsed time once, then return the updated state."
  (('group data state)
    (let* ((desc (proplists:get_value 'desc data 'undefined))
           (time (proplists:get_value 'time data 0))
           (updated-state (record-module-group desc time state)))
      (if (is-file-desc? desc)
          (ltest-formatter:mod-line-end desc time data updated-state))
      updated-state))
  (('test data state)
    (let ((status (proplists:get_value 'status data)))
      (case status
        ('ok
          (ltest-formatter:ok state)
          (set-state-ok state (+ 1 (state-ok state))))
        (`#(skipped ,_reason)
          (io:format (ltest-formatter:skip state))
          (set-state-skip state (+ 1 (state-skip state))))
        (`#(error #(error ,error ,where))
          (case (assertion-failure? error)
            ('true
              (ltest-formatter:fail error where state)
              (set-state-fail state (+ 1 (state-fail state))))
            (_
              (ltest-formatter:err error where state)
              (set-state-err state (+ 1 (state-err state))))))
        (`#(error #(exit ,error ,where))
          (ltest-formatter:err error where state)
          (set-state-err state (+ 1 (state-err state))))
        (`#(error ,error)
          (ltest-formatter:err error 'undefined state)
          (set-state-err state (+ 1 (state-err state))))
        (_
          (ltest-util:rebar-debug "Unexpected EUnit test status: ~p" (list status))
          state)))))

(defun assertion-failure? (reason)
  "Return `true` for EUnit assertion failure tags handled by this listener;
   return `false` for other reasons."
  (andalso (is_tuple reason)
           (> (tuple_size reason) 0)
           (lists:member (element 1 reason)
                         '(assert assertEqual assertNotEqual assertMatch
                           assertNotMatch assertException assertNotException))))

(defun is-file-desc?
  "Return `true` when DESC is a binary beginning with `file `; this checks
   only the prefix, not descriptor completeness."
  (((= (binary (prefix bytes (size 5)) (rest bitstring)) desc)) (when (=:= prefix #"file "))
    'true)
  ((_desc) 'false))

(defun module-for-desc
  "Return the module atom for a valid EUnit file or quoted module
   descriptor; return `undefined` for other prefixes. File/BEAM errors
   propagate."
  (((= (binary (prefix bytes (size 5)) (rest bitstring)) desc)) (when (=:= prefix #"file "))
    (ltest-util:file->module desc))
  (((= (binary (prefix bytes (size 7)) (mod bitstring)) desc)) (when (=:= prefix #"module "))
    (ltest-util:binary-mod->atom mod))
  ((_desc) 'undefined))

(defun record-module-group (desc time state)
  "Add each module's manual skips and TIME once across file/module groups;
   malformed descriptors and BEAM lookup errors propagate."
  (let ((module (module-for-desc desc)))
    (case module
      ('undefined state)
      (_ (case (orddict:find module (state-status state))
           ('error
             (let* ((skips (length (ltest-util:get-skip-tests desc)))
                    (state-with-counts
                      (update-skips (update-time state time) skips)))
               (set-state-status state-with-counts
                                 (orddict:store module 'counted
                                                (state-status state-with-counts)))))
           (_ state))))))

(defun handle_cancel
  "Write the EUnit cancellation reason and increment the cancel counter;
   return the updated listener state."
  ((_group_or_test data state)
    (ltest-formatter:display-test-cancel (proplists:get_value 'reason data))
    (set-state-cancel state (+ 1 (state-cancel state)))))

(defun terminate
  "Write the final report for EUnit's successful terminate callback and
   return its state tuple; on callback error, print the internal error and
   return the listener callback tuple."
  ((`#(ok ,data) (= (match-state color? color?) state))
    (if (> (ltest-util:all-tests state) 0)
        (ltest-formatter:display-results data state)
        (ltest-formatter:display-no-results data state))
    (if (> (proplists:get_value 'cancel data 0) 0)
      (io:format (ltest-color:red
                  "One or more tests were cancelled.\n" color?)))
    ;; EUnit's tty listener owns the {result, Reference, Result} message.
    `#(ok ,data ,state))
  ((`#(error ,reason) state)
    (io:format "Internal EUnit listener error: ~p~n" `(,reason))
    `#(ok ,reason ,state)))

(defun sync_end (result)
  "Return RESULT unchanged; this compatibility helper sends no EUnit
   message and performs no IO."
  result)

(defun update-time (state time) "Add TIME to the elapsed-time field in listener state."
  (set-state-time state (+ time (state-time state))))

(defun update-skips (state skips)
  "Add SKIPS to the skipped-test counter in listener state."
  (set-state-skip state (+ skips (state-skip state))))
