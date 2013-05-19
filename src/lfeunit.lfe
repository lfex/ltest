(defmodule lfeunit
  (export all))

; Define some macro/constants to make up for LFE's lack of ?MODULE and ?LINE
; support.
(defmacro MODULE () `'lfeunit)
(defmacro LINE () `'unknown)

(include-lib "include/lfeunit.lfe")