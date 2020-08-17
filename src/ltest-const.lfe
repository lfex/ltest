(defmodule ltest-const
  (export all))

(defun test-suite-header () "=")
(defun test-suite-subheader () "-")
(defun test-suite-title () "ltest")
(defun test-suite-width () 78)

(defun mod-indent () 0)
(defun func-indent () 2)
(defun error-indent () 6)

(defun defualt-built-dir () "_build")
(defun default-test-profile () 'test)
(defun default-listener () "ltest-listener")
(defun default-test-type () 'unit)

(defun skip-test-patt () ".*_skip")
(defun skip-test-group-patt () "(.*)(_skip)")
