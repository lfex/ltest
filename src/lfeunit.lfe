;;;; The recommended way to use lfeunit is via library inclusion, e.g.:
;;;;    (include-lib "include/lfeunit.lfe")
;;;;
;;;; However, this module allows one to use it as a module, e.g.:
;;;;    (defmodule mymodule_tests
;;;;      export all)
;;;;      (import (from lfeunit (assert-equal 2))))


(defmodule lfeunit
  (export all))

; Define a macro/constants to make up for LFE's lack of ?LINE support.
(defmacro LINE () `'unknown)

(include-lib "include/lfeunit.lfe")