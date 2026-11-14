(defmodule ltest-util
  "Inspect BEAM files and provide shared test-discovery and reporting helpers."
  (export all))

(include-lib "ltest/include/ltest-records.lfe")

(defun file->beam (bin-data)
  "Extract a BEAM path root from a valid quoted EUnit `file ` descriptor;
   the returned path omits the `.beam` suffix."
  (ltest-util:rebar-debug "bin-data (file->beam): ~p" (list bin-data))
  (let* ((`#(,_ ,start) (binary:match bin-data #"file \""))
         (`#(,end ,_) (binary:match bin-data #".beam\""))
         (len (- end start)))
    (ltest-util:rebar-debug "start: ~p, end (file->beam): ~p" (list start end))
    (binary_to_list (binary:part bin-data `#(,start ,len)))))

(defun file->module (bin-data)
  "Read the module atom from the BEAM named by a valid quoted EUnit file
   descriptor; BEAM read errors propagate."
  (ltest-util:rebar-debug "binary? ~p" (list (is_binary bin-data)))
  (ltest-util:rebar-debug "bin-data (file->module): ~p" (list bin-data))
  (beam->module (file->beam bin-data)))
  
(defun beam->module (beam)
  "Read and return the module atom from a readable BEAM path; this does not
   load the module, and BEAM errors propagate."
   (ltest-util:rebar-debug "beam (beam->module): ~p" (list beam))
   (let (((tuple 'ok (tuple module _))
          (beam_lib:chunks beam '())))
     module))

(defun beams->files (beam-data)
  "Append `.beam` to each basename; for `{Module, Basename}` tuples,
   preserve Module and append the suffix to Basename."
  (lists:map
    (match-lambda
      (((tuple mod beam))
        `#(,mod ,(++ beam ".beam")))
      ((beam)
        (++ beam ".beam")))
    beam-data))

(defun beams->modules (beams-list) "Read each module atom from a readable BEAM
   path; read errors propagate."
  (lists:map
    #'beam->module/1
    beams-list))

(defun module->beam (module)
  "Return the resolved code-path BEAM path for MODULE without `.beam`; the
   module must resolve on the current code path."
  (filename:rootname (code:which module)))

(defun modules->beams (module-list)
  "Map modules resolvable on the current code path to unique BEAM path roots."
  (lists:usort
   (lists:map
    #'module->beam/1
    module-list)))

(defun get-behaviour (attrs)
  "Read the behavior attribute, preferring behaviour and falling back to
   behavior; return `undefined` when neither spelling is present."
  (proplists:get_value
    'behaviour
    attrs
    (proplists:get_value 'behavior attrs)))

(defun get-beam-attrs (beam)
  "Read the `attributes` chunk from a readable BEAM file; chunk and read
   errors propagate."
  (let (((tuple 'ok (tuple _ (list (tuple 'attributes attrs))))
         (beam_lib:chunks beam '(attributes))))
    attrs))

(defun get-beam-behaviours (beam)
  "Return the behavior attribute from a readable BEAM; a missing behavior
   attribute yields `[]`, while BEAM read or chunk errors propagate."
  (let ((behavs (get-behaviour (get-beam-attrs beam))))
    (case behavs
      ('undefined '())
      (_ behavs))))

(defun get-beam-exports (beam)
  "Read exported `{Name, Arity}` pairs from a readable BEAM file; read and
   chunk errors propagate."
  (let (((tuple 'ok (tuple _ (list (tuple 'exports exports))))
         (beam_lib:chunks beam '(exports))))
    exports))

(defun filtered (func beams)
  "Apply FUNC to BEAMS and remove `false` results while preserving the
   remaining result order."
  (lists:filter
    (lambda (x) (=/= x 'false))
    (funcall func beams)))

(defun get-module-exports (module)
  "Return exports from the BEAM resolved for MODULE on the current code
   path; the module need not already be loaded."
  (get-beam-exports (code:which module)))

(defun binary-mod->str (mod)
  "Convert the binary bytes to a character list and strip apostrophes at
   both edges; this does not decode UTF-8 or Erlang escapes."
  (string:strip (binary_to_list mod) 'both #\'))

(defun binary-mod->atom (mod)
  "Convert the stripped bytes of a quoted module name to an atom; no UTF-8
   decoding or Erlang escape parsing is performed."
  (list_to_atom (binary-mod->str mod)))

(defun get-skip-tests
  "Read skip exports from a valid EUnit file descriptor or quoted module
   descriptor. Other inputs return `[]`; failures for recognized malformed
   descriptors or unreadable BEAM files propagate."
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
  "Keep only exported `{Name, 0}` pairs recognized by skipped?/1, preserving
   their input order."
  (logger:debug "funcs: ~p" (list funcs))
  (lists:filter #'skipped?/1 funcs))

(defun skipped?
  "For a valid `{Name, Arity}` pair, return `true` only when NAME is an atom
   ending in `_skip` and ARITY is `0`. Intended for exported pairs; other
   input shapes are outside this function's contract."
  ((`#(,func 0))
    (lists:suffix "_skip" (atom_to_list func)))
  ((_)
    'false))

(defun skip-match? "Return `true` for a `{match, ...}` result and `false` for `nomatch`."
  ((`#(match ,_))
    'true)
  (('nomatch)
    'false))

(defun all-tests (state)
  "Sum the `ok`, `skip`, `fail`, `err`, and `cancel` counters in listener
   state."
  (+ (state-ok state)
     (state-skip state)
     (state-fail state)
     (state-err state)
     (state-cancel state)))

(defun rebar-debug (msg)
  "Delegate to `rebar-debug/2` with an empty argument list; when `rebar_api`
   is available that function calls `rebar_api:debug/2`."
  (rebar-debug msg '()))

(defun rebar-debug (msg args)
  "Call `rebar_api:debug/2` with MSG and ARGS when rebar_api is available;
   otherwise return `undefined`."
  (case (code:ensure_loaded 'rebar_api)
    (`#(error ,_)
      'undefined)
    (`#(module ,_)
      (rebar_api:debug msg args))))

(defun get-arg (arg-name default)
  "Return `init:get_argument/1`'s tagged nested result; if absent, return
   `{default, [[Default]]}` rather than Default itself."
  (let ((arg-value (init:get_argument arg-name)))
    (case arg-value
      ('error
        `#(default ((,default))))
      (_ arg-value))))
