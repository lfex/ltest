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
    (io:format "~s~n~n" `(,(++ (string:copies char header-len)
                               title
                               (string:copies char header-len))))))
