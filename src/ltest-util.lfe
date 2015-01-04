;; This module isn't necessary at all; however, it does provide a convenience
;; in the event that you want to do some debugging from the LFE REPL. Since
;; one can't include macros directly in the REPL, this module allows one to
;; still use the macros while in the REPL by doing the following:
;;
;; > (slurp '"src/ltest.lfe")
;;
;; At which point you will be able to call all the macros:
;;
;; > (is-equal 1 1)
;; ok
;; > (is-equal 1 2)
;; exception error: #(assertEqual_failed
;;                    (#(module ltest)
;;                     #(line 1)
;;                     #(expression "2")
;;                     #(expected 1)
;;                     #(value 2)))
;;
(defmodule ltest-util
  (export all))

(defun get-app-src-version (filename)
  (let* (((tuple 'ok (list app)) (file:consult filename)))
    (proplists:get_value 'vsn (element 3 app))))

(defun get-lfe-version ()
  (get-app-src-version (code:where_is_file "lfe.app")))

(defun get-version ()
  `(#(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver-version ,(erlang:system_info 'driver_version))
    #(lfe ,(get-lfe-version))))

(defun get-ltest-version ()
  (get-app-src-version (code:where_is_file "ltest.app")))

(defun get-versions ()
  (++ (get-version)
      `(#(ltest ,(get-ltest-version)))))

