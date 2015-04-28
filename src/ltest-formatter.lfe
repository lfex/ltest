(defmodule ltest-formatter
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun test-suite-header ()
  (io:format (get-suite-header)))

(defun get-suite-header ()
  (ltest-color:greenb
    (test-header (ltest-const:test-suite-title)
                 (ltest-const:test-suite-header))))

(defun test-suite-footer ()
  (io:format (get-suite-footer)))

(defun get-suite-footer ()
  (io_lib:format "~s~n~n" `(,(ltest-color:greenb
                               (string:copies
                                 (ltest-const:test-suite-header)
                                 (ltest-const:test-suite-width))))))

(defun test-type-header (title)
  (io:format
    (ltest-color:blueb
      (test-header title (ltest-const:test-suite-subheader)))))

(defun test-header (title char)
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

(defun indent (count)
  (string:copies " " count))

(defun func-line (raw-func)
  (let ((func (get-func-name raw-func)))
    (io:format "~s~s ~s" `(,(indent (ltest-const:func-indent))
                           ,func
                           ,(get-elision func)))))

(defun get-skip-func-name (func-name)
  (re:replace
    (atom_to_list func-name)
    "_skip$" "" '(global #(return list))))

(defun get-func-name (func-name)
  (re:replace
    (atom_to_list func-name)
    "_test(_)?$" "" '(global #(return list))))


(defun get-elision (func-name)
  (let* ((init-len (+ (ltest-const:func-indent)
                      (length func-name)))
         (end-len (length " [fail]"))
         (elide-len (- (ltest-const:test-suite-width)
                       (+ init-len end-len))))
    (string:copies "." elide-len)))

(defun mod-line (desc)
  (io:format "~smodule: ~s~n"
             `(,(indent (ltest-const:mod-indent))
               ,(ltest-color:greenb
                 (atom_to_list (ltest-util:get-module desc))))))

(defun ok ()
  (io:format (++ "... [" (ltest-color:greenb "ok") "]~n")))

(defun fail (error where)
  (io:format ". [~s]~n~n~s~s:~n" `(,(ltest-color:red "fail")
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb "Assertion failure")))
  (lfe_io:format "~s\e[31;1m~p\e[0m~n~n" `(,(indent (ltest-const:error-indent))
                                           ,error)))

(defun err (error where)
  (io:format " [~s]~n~n~s~s:~n" `(,(ltest-color:yellow "error")
                                   ,(indent (ltest-const:error-indent))
                                   ,(ltest-color:yellowb "Error")))
  (lfe_io:format "~s\e[31;1m~p\e[0m~n~n" `(,(indent (ltest-const:error-indent))
                                           ,error)))

(defun skip ()
  (++ " [" (ltest-color:blue "skip") "]"))

(defun mod-time (time)
  (io:format "~s~s ~s~s~n~n" `(,(indent (ltest-const:func-indent))
                               ,(ltest-color:blackb "time:")
                               ,(ltest-color:blackb (integer_to_list time))
                               ,(ltest-color:blackb "ms"))))

(defun skip-lines (skipped)
  (lists:map #'skip-line/1 skipped))

(defun skip-line
  ((`#(,raw-func ,_))
    (let ((func (get-skip-func-name raw-func)))
      (io:format "~s~s ~s~s~n" `(,(indent (ltest-const:func-indent))
                                 ,func
                                 ,(get-elision func)
                                 ,(skip))))))


(defun display-results (data state)
  (stats-heading)
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

(defun stats-heading ()
  (io:format (++ (ltest-color:yellow "summary:") "~n")))

(defun display-all (state)
  (io:format "~sTests: ~p  " `(,(indent (ltest-const:func-indent))
                               ,(ltest-util:all-tests state))))

(defun display-successes (state)
  (io:format "~s: ~p  " `(,(get-ok-report state)
                          ,(state-ok state))))

(defun display-skips (state)
  (io:format "~s: ~p  " `(,(get-skip-report state)
                          ,(state-skip state))))

(defun display-failures (state)
  (io:format "~s: ~p " `(,(get-fail-report state)
                          ,(state-fail state))))

(defun display-errors (state)
  (io:format "~s: ~p~n" `(,(get-err-report state)
                          ,(state-err state))))

(defun display-pending (state)
  (io:format "Pending state: ~p~n" `(,state)))

(defun display-profile (state)
  (io:format "Profile state: ~p~n" `(,state)))

(defun display-timing (state)
  (io:format "~sTotal time: ~pms~n" `(,(indent (ltest-const:func-indent))
                                    ,(state-time state))))

(defun display-no-results (data state)
  (io:format (ltest-color:yellow (get-no-results-report data state)))
  (finish-section))

(defun get-no-results-report (data state)
  (io_lib:format
    "There were no ~s tests found.~n"
    `(,(state-test-type state))))

(defun finish-section ()
  (io:nl)
  (io:nl))

(defun get-ok-report (state)
  (get-report "Passed" #'ltest-color:greenb/1 (state-ok state)))

(defun get-skip-report (state)
  (get-report "Skipped" #'ltest-color:blue/1 (state-skip state)))

(defun get-fail-report (state)
  (get-report "Failed" #'ltest-color:red/1 (state-fail state)))

(defun get-err-report (state)
  (get-report "Erred" #'ltest-color:yellow/1 (state-err state)))

(defun get-report (text color-func count)
  (if (== count 0)
      text
      (funcall color-func text)))
