(defmodule ltest-util
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun get-version ()
  (rebar3_lfe_version:app_version 'ltest))

(defun get-versions ()
  (rebar3_lfe_version:versions '(ltest)))

(defun get-module (bin-data)
  (lutil-file:beam->module (get-beam bin-data)))

(defun get-beam (bin-data)
  (let* ((`#(,_ ,start) (binary:match bin-data (binary "file \"")))
         (`#(,end ,_) (binary:match bin-data (binary ".beam\"")))
         (len (- end start)))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun get-behaviour (attrs)
  (proplists:get_value
    'behaviour
    attrs
    (proplists:get_value 'behavior attrs)))

(defun get-beam-attrs (beam)
  "Given an atom representing a plugin's name, return its module
  attributes."
  (let (((tuple 'ok (tuple _ (list (tuple 'attributes attrs))))
         (beam_lib:chunks beam '(attributes))))
    attrs))

(defun get-beam-behaviours (beam)
  "Given an atom representing a plugin's name, return its module
  attributes."
  (let ((behavs (get-behaviour (get-beam-attrs beam))))
    (case behavs
      ('undefined '())
      (_ behavs))))

(defun get-beam-exports (beam)
  "Given a beam path, return its exported functions."
  (let (((tuple 'ok (tuple _ (list (tuple 'exports exports))))
         (beam_lib:chunks beam '(exports))))
    exports))

(defun filtered (func beams)
  (lists:filter-files
    (lambda (x) (=/= x 'false))
    (funcall func beams)))

(defun get-module-exports (module)
  "Given an atom representing a module's name, return its exported functions."
  (get-beam-exports (code:which module)))

(defun get-skip-tests (bin-data)
  (filter-skipped
    (get-beam-exports
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

(defun rebar-debug (msg)
  (rebar-debug msg '()))

(defun rebar-debug (msg args)
  "For use in the context of rebar3 plugins."
  (case (code:ensure_loaded 'rebar_api)
    (`#(error ,_)
      'undefined)
    (`#(module ,_)
      (rebar_api:debug msg args))))
