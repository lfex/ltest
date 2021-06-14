(defmodule ltest-util
  (export all))

(include-lib "include/ltest-records.lfe")

(defun file->beam (bin-data)
  (ltest-util:rebar-debug "bin-data (file->beam): ~p" (list bin-data))
  (let* ((`#(,_ ,start) (binary:match bin-data #"file \""))
         (`#(,end ,_) (binary:match bin-data #".beam\""))
         (len (- end start)))
    (ltest-util:rebar-debug "start: ~p, end (file->beam): ~p" (list start end))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun file->module (bin-data)
  (ltest-util:rebar-debug "binary? ~p" (list (is_binary bin-data)))
  (ltest-util:rebar-debug "bin-data (file->module): ~p" (list bin-data))
  (beam->module (file->beam bin-data)))
  
(defun beam->module (beam)
   (ltest-util:rebar-debug "beam (beam->module): ~p" (list beam))
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

(defun module->beam (module)
  (filename:rootname (code:which module)))

(defun modules->beams (module-list)
  (lists:usort
   (lists:map
    #'module->beam/1
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

(defun binary-mod->str (mod)
  (string:strip (binary_to_list mod) 'both #\'))

(defun binary-mod->atom (mod)
  (list_to_atom (binary-mod->str mod)))

(defun get-skip-tests
  (((= (binary (prefix bytes (size 5)) (file bitstring)) bin-data)) (when (=:= prefix #"file "))
   (ltest-util:rebar-debug "Getting skip tests (file) ..." '())
   (filter-skipped
    (get-beam-exports
     (file->beam bin-data))))
  (((binary (prefix bytes (size 7)) (mod bitstring))) (when (=:= prefix #"module "))
   (ltest-util:rebar-debug "Getting skip tests (module) ..." '())
   (filter-skipped
    (get-beam-exports
     (module->beam (binary-mod->atom mod)))))
  ((bin-data)
   (ltest-util:rebar-debug "Getting skip tests (???)..." '())
   (ltest-util:rebar-debug "Unexpected bin-data: ~p" `(,bin-data))
   (ltest-util:rebar-debug "Skipping ..." '())
   '()))

(defun filter-skipped (funcs)
  (logger:debug "funcs: ~p" (list funcs))
  (lists:filter #'skipped?/1 funcs))

(defun skipped?
  ((`#(,func ,_))
    (skip-match?
      (re:run (atom_to_list func) "_skip"))))

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
