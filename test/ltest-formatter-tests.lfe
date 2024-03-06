(defmodule ltest-formatter-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "ltest/include/ltest-records.lfe")

(defun test-file ()
  "_build/test/lib/ltest/ebin/ltest-util.beam")

(defun test-file-data ()
  (list_to_binary (++ "file \"" (test-file) "\"")))

(defun test-module-data ()
  #"module 'ltest-util'")

(deftest mod-line-check-file
  (is-equal "module: ltest-util\n"
            (lists:flatten
             (ltest-formatter:mod-line-check
              (test-file-data)
              (make-state color? 'false)))))

(deftest mod-line-check-module
  (is-equal "module: ltest-util\n"
            (lists:flatten
             (ltest-formatter:mod-line-check
              (test-module-data)
              (make-state color? 'false)))))
