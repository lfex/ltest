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

