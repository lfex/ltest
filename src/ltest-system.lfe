(defmodule ltest-system
  "Define the marker behavior used to select system test modules."
  (export all))

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
