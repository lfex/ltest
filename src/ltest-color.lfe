(defmodule ltest-color
  "Apply optional ANSI foreground and bold color to report text."
  (export all))

(defun black (str)
  "Wrap text in black when coloring is enabled; the one-argument form reads
   the VM color flag."
  (black str (get-color-option)))

(defun blackb (str)
  "Wrap text in bold black when coloring is enabled; the one-argument form
   reads the VM color flag."
  (blackb str (get-color-option)))

(defun white (str)
  "Wrap text in white when coloring is enabled; the one-argument form reads
   the VM color flag."
  (white str (get-color-option)))

(defun whiteb (str)
  "Wrap text in bold white when coloring is enabled; the one-argument form
   reads the VM color flag."
  (whiteb str (get-color-option)))

(defun red (str)
  "Wrap text in red when coloring is enabled; the one-argument form reads
   the VM color flag."
  (red str (get-color-option)))

(defun redb (str)
  "Wrap text in bold red when coloring is enabled; the one-argument form
   reads the VM color flag."
  (redb str (get-color-option)))

(defun yellow (str)
  "Wrap text in yellow when coloring is enabled; the one-argument form
   reads the VM color flag."
  (yellow str (get-color-option)))

(defun yellowb (str)
  "Wrap text in bold yellow when coloring is enabled; the one-argument form
   reads the VM color flag."
  (yellowb str (get-color-option)))

(defun green (str)
  "Wrap text in green when coloring is enabled; the one-argument form reads
   the VM color flag."
  (green str (get-color-option)))

(defun greenb (str)
  "Wrap text in bold green when coloring is enabled; the one-argument form
   reads the VM color flag."
  (greenb str (get-color-option)))

(defun blue (str)
  "Wrap text in blue when coloring is enabled; the one-argument form reads
   the VM color flag."
  (blue str (get-color-option)))

(defun blueb (str)
  "Wrap text in bold blue when coloring is enabled; the one-argument form
   reads the VM color flag."
  (blueb str (get-color-option)))

(defun get-color-option ()
  "Read the first color init-argument value as an atom; use `true` when
   the argument is absent."
  (clj:->> (ltest-util:get-arg 'color "true")
           (element 2)
           (caar)
           (list_to_atom)))

(defun black (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:black str)
    str))

(defun blackb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:blackb str)
    str))

(defun white (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:white str)
    str))

(defun whiteb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:whiteb str)
    str))

(defun red (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:red str)
    str))

(defun redb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:redb str)
    str))

(defun yellow (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:yellow str)
    str))

(defun yellowb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:yellowb str)
    str))

(defun green (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:green str)
    str))

(defun greenb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:greenb str)
    str))

(defun blue (str color?)
  "For valid iodata STR, return color2.0's nested ANSI iodata when COLOR?
   is true; otherwise return STR unchanged."
  (if color?
    (color:blue str)
    str))

(defun blueb (str color?)
  "For valid iodata STR, return color2.0's nested bold ANSI iodata when
   COLOR? is true; otherwise return STR unchanged."
  (if color?
    (color:blueb str)
    str))
