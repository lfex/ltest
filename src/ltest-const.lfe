(defmodule ltest-const
  "Expose fixed text and indentation values used by the ltest reporter."
  (export all))

(defun test-suite-header ()
  "Return the top-level report rule character `=`."
  "=")
(defun test-suite-subheader ()
  "Return the subsection report rule character `-`."
  "-")
(defun test-suite-title () "Return the fixed report title `ltest`." "ltest")
(defun test-suite-width ()
  "Return the fixed report title and heading width, `72` characters."
  72)
(defun mod-indent () "Return the module report indentation, `0` spaces." 0)
(defun func-indent ()
  "Return the function report indentation, `2` spaces."
  2)
(defun error-indent ()
  "Return the assertion/error detail indentation, `6` spaces."
  6)
