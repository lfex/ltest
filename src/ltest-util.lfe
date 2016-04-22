(defmodule ltest-util
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun get-version ()
  (lr3-ver-util:get-app-version 'ltest))

(defun get-versions ()
  (++ (lr3-ver-util:get-versions)
      `(#(kla ,(lr3-ver-util:get-app-version 'kla))
        #(clj ,(lr3-ver-util:get-app-version 'clj))
        #(lutil ,(lr3-ver-util:get-app-version 'lutil))
        #(ltest ,(get-version)))))

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
