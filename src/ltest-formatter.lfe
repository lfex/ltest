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
                                   ,(color:yellow "Error")))
  (lfe_io:format "~s\e[31;1m~p\e[0m~n~n" `(,(indent (ltest-const:error-indent))
                                           ,error)))

(defun mod-time (time)
  (io:format "~s~s ~s~s~n~n" `(,(indent (ltest-const:func-indent))
                               ,(color:blackb "time:")
                               ,(color:blackb (integer_to_list time))
                               ,(color:blackb "ms"))))
