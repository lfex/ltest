(defmodule ltest-listener
  (behaviour eunit_listener)
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun start ()
  (start '()))

(defun start (options)
  (eunit_listener:start (MODULE) options))

(defun init (options)
  (make-state options options))

(defun handle_begin
  (('group (= `(,_ #(desc undefined) ,_ ,_) data) state)
    state)
  (('group (= `(,_ #(desc ,desc) ,_ ,_) data) state)
    (case (binary:match desc (binary "file"))
      ('nomatch 'skipping)
      (_ (ltest-formatter:mod-line desc)))
    ; (io:format "\tdata: ~p~n" (list data))
    ; (io:format "\tstate: ~p~n" (list state))
    state)
  (('test (= `(,_ ,_ #(source #(,mod ,func ,arity)) ,_) data) state)
    (ltest-formatter:func-line func)
    ;(io:format "\t\tdata: ~p~n" (list data))
    ;(io:format "\t\tstate: ~p~n" (list state))
    state)
  (('test data state)
    (io:format "\tstarting test ...~n")
    ;(io:format "\t\tdata: ~p~n" (list data))
    ;(io:format "\t\tstate: ~p~n" (list state))
    state)
  )

(defun handle_end
  (('group `(,_ ,_ #(desc undefined) ,_ ,_ #(time ,time) ,_) state)
    ; skipping undefined description
    state)
  (('group (= `(,_ ,_ #(desc ,desc) ,_ ,_ #(time ,time) ,_) data) state)
    (case (binary:match desc (binary "file"))
      ('nomatch 'skipping)
      (_ (ltest-formatter:mod-time time)))
    ;(io:format "ending group ...~n")
    ;(io:format "\tdata: ~p~n" (list data))
    ;(io:format "\tstate: ~p~n" (list state))
    (set-state-time state (+ time (state-time state))))
  (('group data state)
    ;(io:format "ending group ...~n")
    ;(io:format "\tdata: ~p~n" (list data))
    ;(io:format "\tstate: ~p~n" (list state))
    state)
  (('test (= `(,_ #(status #(error #(error ,error ,where))) ,_ ,_ ,_ ,_ ,_) data) state)
    (ltest-formatter:fail error where)
    (set-state-fail state (+ 1 (state-fail state))))
  (('test `(,_ #(status ok) ,_ ,_ ,_ ,_ ,_) state)
    (ltest-formatter:ok)
    (set-state-ok state (+ 1 (state-ok state))))
  (('test data state)
    (io:format "\tending test ...~n")
    (io:format "\t\tdata: ~p~n" `(,data))
    (io:format "\t\tstate: ~p~n" `(,state))
    state))

(defun handle_cancel
  ((_ data state)
    state))

(defun terminate
  ((`#(ok ,data) state)
    ;(io:format "Terminating ...~n")
    (if (> (ltest-util:all-tests state) 0)
        (ltest-formatter:display-results data state)
        (ltest-formatter:display-no-results data state))
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


