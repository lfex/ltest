(defmodule ltest-util
  (export all))

(include-lib "include/ltest-records.lfe")

(defun get-module (bin-data)
  (beam->module (get-beam bin-data)))

(defun get-beam (bin-data)
  (let* ((`#(,_ ,start) (binary:match bin-data (binary "file \"")))
         (`#(,end ,_) (binary:match bin-data (binary ".beam\"")))
         (len (- end start)))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun beam->module (beam)
  (let (((tuple 'ok (tuple module _))
         (beam_lib:chunks beam '())))
    module))

(defun beams->files (beam-data)
  "Given a list of beams (no .beam extension), return a list of files (with
  the .beam extension)."
  (lists:map
    (match-lambda
      (((tuple mod beam))
        `#(,mod ,(++ beam ".beam")))
      ((beam)
        (++ beam ".beam")))
    beam-data))

(defun beams->modules (beams-list)
  (lists:map
    #'beam->module/1
    beams-list))

(defun modules->beams (module-list)
  (lists:usort
    (lists:map
      (lambda (x)
        (filename:rootname (code:which x)))
      module-list)))

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

(defun get-arg (arg-name default)
  (let ((arg-value (init:get_argument arg-name)))
    (case arg-value
      ('error
        `#(default ((,default))))
      (_ arg-value))))
