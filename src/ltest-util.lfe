(defmodule ltest-util
  (export all))

(defun get-app-src-version (name)
  (let* ((filename (code:where_is_file name))
         ((tuple 'ok (list app)) (file:consult filename)))
    (proplists:get_value 'vsn (element 3 app))))

(defun get-lfe-version ()
  (get-app-src-version "lfe.app"))

(defun get-version ()
  (get-app-src-version "ltest.app"))

(defun get-versions ()
  `(#(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver-version ,(erlang:system_info 'driver_version))
    #(lfe ,(get-lfe-version))
    #(ltest ,(get-version))))

