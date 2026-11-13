(defmodule ltest-util
  "Inspect BEAM files and provide shared test-discovery and reporting helpers."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun file->beam (bin-data)
  "Extract the BEAM path from a compiler description binary containing a
   file path and .beam suffix."
  (ltest-util:rebar-debug "bin-data (file->beam): ~p" (list bin-data))
  (let* ((`#(,_ ,start) (binary:match bin-data #"file \""))
         (`#(,end ,_) (binary:match bin-data #".beam\""))
         (len (- end start)))
    (ltest-util:rebar-debug "start: ~p, end (file->beam): ~p" (list start end))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun file->module (bin-data)
  "Extract the module atom from a compiler description binary containing a
   file path and .beam suffix."
  (ltest-util:rebar-debug "binary? ~p" (list (is_binary bin-data)))
  (ltest-util:rebar-debug "bin-data (file->module): ~p" (list bin-data))
  (beam->module (file->beam bin-data)))
  
(defun beam->module (beam)
  "Read and return the module atom recorded in a BEAM file."
   (ltest-util:rebar-debug "beam (beam->module): ~p" (list beam))
   (let (((tuple 'ok (tuple module _))
          (beam_lib:chunks beam '())))
     module))

(defun beams->files (beam-data)
  "Append the .beam suffix to BEAM basenames, preserving {module, basename}
   pairs."
  (lists:map
    (match-lambda
      (((tuple mod beam))
        `#(,mod ,(++ beam ".beam")))
      ((beam)
        (++ beam ".beam")))
    beam-data))

(defun beams->modules (beams-list) "Map BEAM paths to their module atoms."
  (lists:map
    #'beam->module/1
    beams-list))

(defun module->beam (module)
  "Return the loaded BEAM path without its .beam suffix."
  (filename:rootname (code:which module)))

(defun modules->beams (module-list)
  "Map module atoms to unique loaded BEAM basenames."
  (lists:usort
   (lists:map
    #'module->beam/1
    module-list)))

(defun get-behaviour (attrs)
  "Read the behavior attribute, preferring behaviour and falling back to
   behavior."
  (proplists:get_value
    'behaviour
    attrs
    (proplists:get_value 'behavior attrs)))

(defun get-beam-attrs (beam)
  "Read and return the attributes chunk from a BEAM file."
  (let (((tuple 'ok (tuple _ (list (tuple 'attributes attrs))))
         (beam_lib:chunks beam '(attributes))))
    attrs))

(defun get-beam-behaviours (beam)
  "Return a BEAM file behavior attribute as a list; missing attributes
   yield an empty list."
  (let ((behavs (get-behaviour (get-beam-attrs beam))))
    (case behavs
      ('undefined '())
      (_ behavs))))

(defun get-beam-exports (beam)
  "Read and return the exported name/arity pairs from a BEAM file."
  (let (((tuple 'ok (tuple _ (list (tuple 'exports exports))))
         (beam_lib:chunks beam '(exports))))
    exports))

(defun filtered (func beams)
  "Apply a list-transforming helper and remove its false sentinel values."
  (lists:filter
    (lambda (x) (=/= x 'false))
    (funcall func beams)))

(defun get-module-exports (module)
  "Return the exports of the module at its currently loaded code path."
  (get-beam-exports (code:which module)))

(defun binary-mod->str (mod)
  "Convert a quoted module-name binary to a stripped character list."
  (string:strip (binary_to_list mod) 'both #\'))

(defun binary-mod->atom (mod)
  "Convert a quoted module-name binary to a module atom."
  (list_to_atom (binary-mod->str mod)))

(defun get-skip-tests
  "Read skip exports from a rebar file or module description; unsupported
   input returns an empty list."
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
  "Keep exported functions recognized by skipped?/1."
  (logger:debug "funcs: ~p" (list funcs))
  (lists:filter #'skipped?/1 funcs))

(defun skipped?
  "Return true only for a nullary function whose name ends in _skip."
  ((`#(,func 0))
    (lists:suffix "_skip" (atom_to_list func)))
  ((_)
    'false))

(defun skip-match? "Convert a regular-expression match result to a boolean."
  ((`#(match ,_))
    'true)
  (('nomatch)
    'false))

(defun all-tests (state)
  "Sum completed, skipped, failed, errored, and cancelled counts from
   listener state."
  (+ (state-ok state)
     (state-skip state)
     (state-fail state)
     (state-err state)
     (state-cancel state)))

(defun rebar-debug (msg)
  "Send a debug message through rebar_api when that module is loaded;
   otherwise return undefined."
  (rebar-debug msg '()))

(defun rebar-debug (msg args)
  "Send a debug message through rebar_api when that module is loaded;
   otherwise return undefined."
  (case (code:ensure_loaded 'rebar_api)
    (`#(error ,_)
      'undefined)
    (`#(module ,_)
      (rebar_api:debug msg args))))

(defun get-arg (arg-name default)
  "Return an init argument value or the supplied default when the argument
   is absent."
  (let ((arg-value (init:get_argument arg-name)))
    (case arg-value
      ('error
        `#(default ((,default))))
      (_ arg-value))))
