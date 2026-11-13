(defmodule ltest-const
  "Expose fixed text and indentation values used by the ltest reporter."
  (export all))

(defun test-suite-header ()
  "Return the character used for the top-level report rule."
  "=")
(defun test-suite-subheader ()
  "Return the character used for the report subsection rule."
  "-")
(defun test-suite-title () "Return the fixed ltest report title." "ltest")
(defun test-suite-width ()
  "Return the report title and padding width in characters."
  72)
(defun mod-indent () "Return the indentation width for module report lines." 0)
(defun func-indent ()
  "Return the indentation width for function report lines."
  2)
(defun error-indent ()
  "Return the indentation width for assertion and error details."
  6)
