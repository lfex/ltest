(defmodule ltest-util
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun get-app-version
  ((name) (when (is_atom name))
    (get-app-src-version
      (code:where_is_file (++ (atom_to_list name) ".app"))))
  ((name) (error "App name must be an atom.")))

(defun get-app-src-version (filename)
  "Deprecated; kept for projects still using it."
  (let (((tuple 'ok (list app)) (file:consult filename)))
    (proplists:get_value 'vsn (element 3 app))))

(defun get-lfe-version ()
  (get-app-version 'lfe))

(defun get-version ()
  (get-app-version 'ltest))

(defun get-versions ()
  `(#(erlang ,(erlang:system_info 'otp_release))
    #(emulator ,(erlang:system_info 'version))
    #(driver-version ,(erlang:system_info 'driver_version))
    #(lfe ,(get-lfe-version))
    #(ltest ,(get-version))))

(defun get-module (bin-data)
  (lutil-file:beam->module (get-beam bin-data)))

(defun get-beam (bin-data)
  (let* ((`#(,_ ,start) (binary:match bin-data (binary "file \"")))
         (`#(,end ,_) (binary:match bin-data (binary ".beam\"")))
         (len (- end start)))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun get-skip-tests (bin-data)
  (filter-skipped
    (lutil-file:get-beam-exports
      (get-beam bin-data))))

(defun filter-skipped (funcs)
  (lists:filter #'skipped?/1 funcs))

(defun skipped?
  ((`#(,func ,_))
    (skip-match?
      (re:run (atom_to_list func) "_skip$"))))

(defun skip-match?
  ((`#(match ,_))
    'true)
  (('nomatch)
    'false))

(defun all-tests (state)
  (+ (state-ok state)
     (state-skip state)
     (state-fail state)
     (state-cancel state)))
