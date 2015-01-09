(defmodule ltest-formatter
  (export all))

(defun display-failures (state)
  'noop)

(defun display-pending (state)
  'noop)

(defun display-profile (state)
  'noop)

(defun display-timing (state)
  'noop)

(defun display-results (data state)
  'noop)

(defun test-suite-header ()
  (test-header (ltest-const:test-suite-title)
               (ltest-const:test-suite-header)))

(defun test-suite-footer ()
  (io:format "~s~n~n" `(,(string:copies
                           (ltest-const:test-suite-header)
                           (ltest-const:test-suite-width)))))

(defun test-type-header (title)
  (test-header title (ltest-const:test-suite-subheader)))

(defun test-header (title char)
  (let* ((title (++ " " title " "))
         (width (ltest-const:test-suite-width))
         (header-len (trunc (/ (- width (length title)) 2))))
    (io:format "~s~n~n" `(,(color:blue (++ (string:copies char header-len)
                                       title
                                       (string:copies char header-len)))))))
(defun indent (count)
  (string:copies " " count))

(defun func-line (func)
  (io:format "~s~s ..." `(,(indent (ltest-const:func-indent)) ,func)))

(defun mod-line (desc)
  (io:format "~smodule: ~s~n"
             `(,(indent (ltest-const:mod-indent))
               ,(color:greenb (atom_to_list (ltest-util:get-module desc))))))

(defun ok ()
  (io:format (++ ".. [" (color:greenb "ok") "]~n")))

(defun fail (error where)
  (io:format (++ " [" (color:red "fail") "]~n~n\tError:\n"))
  (lfe_io:format "\t\e[31m~p\e[0m~n~n" `(,error)))

(defun mod-time (time)
  (io:format "~s~s ~s~s~n~n" `(,(indent (ltest-const:func-indent))
                               ,(color:blackb "time:")
                               ,(color:blackb (integer_to_list time))
                               ,(color:blackb "ms"))))
