(defmodule ltest-util-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun test-file ()
  "_build/test/lib/ltest/ebin/ltest-util.beam")

(defun test-file-data ()
  (list_to_binary (++ "file \"" (test-file) "\"")))

(deftest file->beam
  (is-equal "_build/test/lib/ltest/ebin/ltest-util"
            (ltest-util:file->beam (test-file-data))))

(deftest file->module
  (is-equal 'ltest-util
            (ltest-util:file->module (test-file-data))))
