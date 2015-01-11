(defmodule ltest-formatter
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun test-suite-header ()
  (io:format (get-suite-header)))

(defun get-suite-header ()
  (color:greenb
    (test-header (ltest-const:test-suite-title)
                 (ltest-const:test-suite-header))))

(defun test-suite-footer ()
  (io:format (get-suite-footer)))

(defun get-suite-footer ()
  (io_lib:format "~s~n~n" `(,(color:greenb
                               (string:copies
                                 (ltest-const:test-suite-header)
                                 (ltest-const:test-suite-width))))))

(defun test-type-header (title)
  (io:format
    (color:blueb
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

(defun func-line (func)
  (io:format "~s~s ~s" `(,(indent (ltest-const:func-indent))
                         ,func
                         ,(get-elision func))))

(defun get-elision (func-name)
  (let* ((init-len (+ (ltest-const:func-indent)
                      (length (atom_to_list func-name))))
         (end-len (length " [fail]"))
         (elide-len (- (ltest-const:test-suite-width)
                       (+ init-len end-len 1))))
    (string:copies "." elide-len)))

(defun mod-line (desc)
  (io:format "~smodule: ~s~n"
             `(,(indent (ltest-const:mod-indent))
               ,(color:greenb (atom_to_list (ltest-util:get-module desc))))))

(defun ok ()
  (io:format (++ ".. [" (color:greenb "ok") "]~n")))

(defun fail (error where)
  (io:format " [~s]~n~n~s~s:~n" `(,(color:red "fail")
                                   ,(indent (ltest-const:error-indent))
                                   ,(color:yellowb "Error")))
  (lfe_io:format "~s\e[31;1m~p\e[0m~n~n" `(,(indent (ltest-const:error-indent))
                                           ,error)))

(defun mod-time (time)
  (io:format "~s~s ~s~s~n~n" `(,(indent (ltest-const:func-indent))
                               ,(color:blackb "time:")
                               ,(color:blackb (integer_to_list time))
                               ,(color:blackb "ms"))))

(defun display-results (data state)
  (stats-heading)
  (display-all state)
  (display-successes state)
  (display-skips state)
  (display-failures state)
  ;;(ltest-formatter:display-pending state)
  ;;(ltest-formatter:display-profile state)
  (display-timing state)
  ;;(ltest-formatter:display-results data state)
  (finish-section))

(defun stats-heading ()
  (io:format (++ (color:yellow "summary:") "~n")))

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
  (io:format "~s: ~p~n" `(,(get-fail-report state)
                          ,(state-fail state))))

(defun display-pending (state)
  (io:format "Pending state: ~p~n" `(,state)))

(defun display-profile (state)
  (io:format "Profile state: ~p~n" `(,state)))

(defun display-timing (state)
  (io:format "~sTotal time: ~pms~n" `(,(indent (ltest-const:func-indent))
                                    ,(state-time state))))

(defun display-no-results (data state)
  (io:format (color:yellow (get-no-results-report data state)))
  (finish-section))

(defun get-no-results-report (data state)
  (io_lib:format
    "There were no ~s tests found.~n"
    `(,(state-test-type state))))

(defun finish-section ()
  (io:nl)
  (io:nl))

(defun get-ok-report (state)
  (get-report "Passed" #'color:greenb/1 (state-ok state)))

(defun get-skip-report (state)
  (get-report "Skipped" #'color:blue/1 (state-skip state)))

(defun get-fail-report (state)
  (get-report "Failed" #'color:red/1 (state-fail state)))

(defun get-report (text color-func count)
  (if (== count 0)
      text
      (funcall color-func text)))
