(defmodule ltest-integration
  "Define the marker behavior used to select integration test modules."
  (export
   (behaviour_info 1)
   (get-modules 0)))

(defun behaviour_info
  "Report no callbacks for the marker behavior; unsupported queries return
   undefined."
  (('callbacks)
    '())
  ((_)
    'undefined))

(defun get-modules ()
  "Return the placeholder ok value; test discovery is performed by ltest."
  'ok)
