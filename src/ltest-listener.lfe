(defmodule ltest-listener
  (behaviour eunit_listener)
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun start ()
  (start '()))

(defun start (options)
  (eunit_listener:start (MODULE) options))

(defun init (options)
  (make-state
   status (orddict:new)
   test-type (proplists:get_value 'test-type options)
   color?  (proplists:get_value 'color options)))

(defun handle_begin
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
  "Render EUnit end events and accumulate one-time group timing."
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
  "Return true for EUnit's assertion-specific error reason tags."
  (andalso (is_tuple reason)
           (> (tuple_size reason) 0)
           (lists:member (element 1 reason)
                         '(assert assertEqual assertNotEqual assertMatch
                           assertNotMatch assertException assertNotException))))

(defun is-file-desc?
  "Return true for a complete EUnit file descriptor."
  (((= (binary (prefix bytes (size 5)) (rest bitstring)) desc)) (when (=:= prefix #"file "))
    'true)
  ((_desc) 'false))

(defun module-for-desc
  "Return the module represented by an EUnit file or module descriptor."
  (((= (binary (prefix bytes (size 5)) (rest bitstring)) desc)) (when (=:= prefix #"file "))
    (ltest-util:file->module desc))
  (((= (binary (prefix bytes (size 7)) (mod bitstring)) desc)) (when (=:= prefix #"module "))
    (ltest-util:binary-mod->atom mod))
  ((_desc) 'undefined))

(defun record-module-group (desc time state)
  "Count manual skips and elapsed time once per module across file/module groups."
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
  "Display EUnit cancellation and count it separately from errors."
  ((_group_or_test data state)
    (ltest-formatter:display-test-cancel (proplists:get_value 'reason data))
    (set-state-cancel state (+ 1 (state-cancel state)))))

(defun terminate
  "Render final listener output without competing with EUnit's result sender."
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
  "Legacy compatibility helper; EUnit's tty remains the sole result sender."
  result)

(defun update-time (state time)
  (set-state-time state (+ time (state-time state))))

(defun update-skips (state skips)
  (set-state-skip state (+ skips (state-skip state))))
