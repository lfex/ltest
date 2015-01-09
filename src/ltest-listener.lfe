(defmodule ltest-listener
  (behaviour eunit_listener)
  (export all))

(defrecord state
  (status (orddict:new))
  (pass 0)
  (fail 0)
  (skip 0)
  (cancel 0)
  (timings '()))

(defun start ()
  (start '()))

(defun start (options)
  (eunit_listener:start (MODULE) options))

(defun init (options)
  (make-state))

(defun handle_begin
  (('group (= `(,_ #(desc undefined) ,_ ,_) data) state)
    'skipping-undefined-desc)
  (('group (= `(,_ #(desc ,desc) ,_ ,_) data) state)
    (case (binary:match desc (binary "file"))
      ('nomatch 'skipping)
      (_ (io:format "~s:~n" `(,(ltest-util:get-module desc)))))
    ; (io:format "\tdata: ~p~n" (list data))
    ; (io:format "\tstate: ~p~n" (list state))
    state)
  (('test (= `(,_ ,_ #(source #(,mod ,func ,arity)) ,_) data) state)
    (io:format "\t~s ... " `(,func))
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
      (_ (io:format "\t~s ~s~s~n~n" `(,(color:blackb "time:")
                                      ,(color:blackb (integer_to_list time))
                                      ,(color:blackb "ms")))))
    ;(io:format "ending group ...~n")
    ;(io:format "\tdata: ~p~n" (list data))
    ;(io:format "\tstate: ~p~n" (list state))
    state)
  (('group data state)
    ;(io:format "ending group ...~n")
    ;(io:format "\tdata: ~p~n" (list data))
    ;(io:format "\tstate: ~p~n" (list state))
    state)
  (('test (= `(,_ #(status #(error #(error ,error ,where))) ,_ ,_ ,_ ,_ ,_) data) state)
    (io:format (++ (color:red "FAIL") "~n~n\tError:\n"))
    (lfe_io:format "\t\e[31m~p\e[0m~n~n" `(,error))
    state)
  (('test `(,_ #(status ok) ,_ ,_ ,_ ,_ ,_) state)
    (io:format (++ (color:greenb "OK") "~n"))
    state)
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
    (ltest-formatter:display-failures state)
    (ltest-formatter:display-pending state)
    (ltest-formatter:display-profile state)
    (ltest-formatter:display-timing state)
    (ltest-formatter:display-results data state))
  ((`#(error ,reason) state)
    (io:nl)
    (io:nl)
    (sync_end 'error)))

(defun sync_end (result)
  (receive
    (`#(stop ,reference ,reply-to)
      (! reply-to `#(result ,reference ,result))
      'ok)))


