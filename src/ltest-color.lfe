(defmodule ltest-color
  (export all))

(include-lib "clj/include/compose.lfe")

(defun black (str)
  (case (get-color-option)
    ('false str)
    ('true (color:black str))))

(defun blackb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:blackb str))))

(defun white (str)
  (case (get-color-option)
    ('false str)
    ('true (color:white str))))

(defun whiteb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:whiteb str))))

(defun red (str)
  (case (get-color-option)
    ('false str)
    ('true (color:red str))))

(defun redb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:redb str))))

(defun yellow (str)
  (case (get-color-option)
    ('false str)
    ('true (color:yellow str))))

(defun yellowb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:yellowb str))))

(defun green (str)
  (case (get-color-option)
    ('false str)
    ('true (color:green str))))

(defun greenb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:greenb str))))

(defun blue (str)
  (case (get-color-option)
    ('false str)
    ('true (color:blue str))))

(defun blueb (str)
  (case (get-color-option)
    ('false str)
    ('true (color:blueb str))))

(defun get-color-option ()
  (->> (lutil-file:get-arg 'color "true")
       (element 2)
       (caar)
       (list_to_atom)))

