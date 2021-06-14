(defmodule ltest-listener
  (behaviour eunit_listener)
  (export all))

(include-lib "include/ltest-records.lfe")

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
  (('group `(,_ ,_ #(desc undefined) ,_ ,_ #(time ,time) ,_) state)
    ;; skipping undefined description
    state)
  (('group (= `(,_ ,_ #(desc ,desc) ,_ ,_ #(time ,time) ,_) data) state)
    (case (binary:match desc (binary "file"))
      ('nomatch state)
      (_ (ltest-formatter:mod-line-end desc time data state)))
    (update-skips
     (update-time state time)
     (length (ltest-util:get-skip-tests desc))))
  (('group data state)
    state)
  (('test (= `(,_ #(status #(error #(error ,error ,where))) ,_ ,_ ,_ ,_ ,_) data) state)
    (ltest-formatter:fail error where state)
    (set-state-fail state (+ 1 (state-fail state))))
  (('test (= `(,_ #(status #(error #(exit ,error ,where))) ,_ ,_ ,_ ,_ ,_) data) state)
    (ltest-formatter:err
      (element 2 (proplists:get_value 'status data)) where state)
    ;; XXX maybe change this to set-state-cancel?
    (set-state-err state (+ 1 (state-err state))))
  (('test `(,_ #(status ok) ,_ ,_ ,_ ,_ ,_) state)
    (ltest-formatter:ok state)
    (set-state-ok state (+ 1 (state-ok state))))
  ((type data state)
   (ltest-util:rebar-debug "*** UNEXPECTED ***" '())
   (ltest-util:rebar-debug "type: ~p~ndata: ~p~n: state: ~p" (list type data state))
   state))

(defun handle_cancel
  ((_group_or_test data state)
    (ltest-formatter:display-test-cancel (proplists:get_value 'reason data))
    ;; This counts groups and tests together
    (set-state-err state (+ 1 (state-err state)))))

(defun terminate
  ((`#(ok ,data) (= (match-state color? color?) state))
    ;; (io:format "Terminating ...~n")
    (if (> (ltest-util:all-tests state) 0)
        (ltest-formatter:display-results data state)
        (ltest-formatter:display-no-results data state))
    ;; error out if tests were cancelled
    (if (> (proplists:get_value 'cancel data 0) 0)
      (io:format (ltest-color:red
                  "One or more tests were cancelled.\n" color?))
        (sync_end 'error)) ;same behaviour as eunit
    `#(ok ,data ,state))
  ((`#(error ,reason) state)
    (io:nl)
    (io:nl)
    (sync_end `#(error ,reason ,state))))

(defun sync_end (result)
  (receive
    (`#(stop ,reference ,reply-to)
      (! reply-to `#(result ,reference ,result))
      `#(ok ,result))))

(defun update-time (state time)
  (set-state-time state (+ time (state-time state))))

(defun update-skips (state skips)
  (set-state-skip state (+ skips (state-skip state))))
