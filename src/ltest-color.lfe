(defmodule ltest-color
  (export all))

(defun black (str)
  (black str (get-color-option)))

(defun blackb (str)
  (blackb str (get-color-option)))

(defun white (str)
  (white str (get-color-option)))

(defun whiteb (str)
  (whiteb str (get-color-option)))

(defun red (str)
  (red str (get-color-option)))

(defun redb (str)
  (redb str (get-color-option)))

(defun yellow (str)
  (yellow str (get-color-option)))

(defun yellowb (str)
  (yellowb str (get-color-option)))

(defun green (str)
  (green str (get-color-option)))

(defun greenb (str)
  (greenb str (get-color-option)))

(defun blue (str)
  (blue str (get-color-option)))

(defun blueb (str)
  (blueb str (get-color-option)))

(defun get-color-option ()
  (clj:->> (ltest-util:get-arg 'color "true")
           (element 2)
           (caar)
           (list_to_atom)))

(defun black (str color?)
  (if color?
    (color:black str)
    str))

(defun blackb (str color?)
  (if color?
    (color:blackb str)
    str))

(defun white (str color?)
  (if color?
    (color:white str)
    str))

(defun whiteb (str color?)
  (if color?
    (color:whiteb str)
    str))

(defun red (str color?)
  (if color?
    (color:red str)
    str))

(defun redb (str color?)
  (if color?
    (color:redb str)
    str))

(defun yellow (str color?)
  (if color?
    (color:yellow str)
    str))

(defun yellowb (str color?)
  (if color?
    (color:yellowb str)
    str))

(defun green (str color?)
  (if color?
    (color:green str)
    str))

(defun greenb (str color?)
  (if color?
    (color:greenb str)
    str))

(defun blue (str color?)
  (if color?
    (color:blue str)
    str))

(defun blueb (str color?)
  (if color?
    (color:blueb str)
    str))
