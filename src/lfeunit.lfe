;; This module isn't necessary at all; however, it does provide a convenience
;; in the event that you want to do some debugging from the LFE REPL. Since
;; one can't include macros directly in the REPL, this module allows one to
;; still use the macros while in the REPL by doing the following:
;;
;; > (slurp '"src/lfeunit.lfe")
;;
;; At which point you will be able to call all the macros:
;;
;; > (is-equal 1 1)
;; ok
;; > (is-equal 1 2)
;; exception error: #(assertEqual_failed
;;                    (#(module lfeunit)
;;                     #(line 1)
;;                     #(expression "2")
;;                     #(expected 1)
;;                     #(value 2)))
;;
(defmodule lfeunit
  (export all))

(include-lib "include/lfeunit-macros.lfe")


(defun noop ()
  'ok)
