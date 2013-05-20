;;;; This file is intended to be used via inclusion, not as a module (thus
;;;; there is no module definition). To use these functions to power your unit
;;;; tests, simply include the source file:
;;;;    (include-lib "src/lfeunit.lfe")
;;;;
;;;; This assumes that you have the lfeunit source code directory in your
;;;; ERL_LIBS environment variable or that lfeunit is installed system-wide.
(include-lib "include/lfeunit-const.lfe")
(include-lib "include/lfeunit-util.lfe")

(defun assert (bool-expression)
  "
  This function takes an expression that returns a boolean value. If the
  expression does not evaluate as a truth value, an error is returned.
  "
  (let* ((check (not (not (eval bool-expression))))
         (data (get-failure-data 'true bool-expression)))
    (case check
      ('true 'ok)
      ('false (: erlang error (tuple 'assert_failed data))))))


(defun assert-not (bool-expression)
  "
  This function takes an expression that returns a boolean value. If the
  expression does not evaluate as false value, an error is returned.
  "
  (let* ((check (not (not (eval bool-expression))))
         (data (get-failure-data 'false bool-expression)))
    (case check
      ('false 'ok)
      ('true (: erlang error (tuple 'assert-not_failed data))))))

(defun assert-equal (value expression)
  "
  This function checks the equality between a passed value and a quoated
  expression.
  "
  (let ((evaled-expr (eval expression))
        (data (get-failure-data value expression)))
    (cond
      ((== value evaled-expr)
       'ok)
      ('true (: erlang error (tuple 'assert-equal_failed data))))))

(defun assert-not-equal (value expression)
  "
  This function checks the inequality between an expected value and a passed
  expression.
  "
  (let ((evaled-expr (eval expression))
        (data (get-failure-data value expression)))
    (cond
      ((/= value evaled-expr)
       'ok)
      ('true (: erlang error (tuple 'assert-not-equal_failed data))))))

(defun assert-exception (expected-class expected-term expression)
  "
  This function check that the passeed expression raises the expected exception
  class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef, 'badarith, etc.).
  "
  (let* ((fail 'assert-exception_failed)
         (succeed 'unexpected-success)
         (data (get-exception-data expected-class expected-term expression)))
    (try
      (progn
        (eval expression)
        (: erlang error succeed))
      (catch
        ((tuple actual-class actual-term _)
          (cond
            ; did we get the expected exception class and term?
            ((and (== actual-class expected-class)
                  (== actual-term expected-term))
              'ok)
            ; did we get a success eval instead of an exception?
            ((== actual-term succeed)
              (: erlang error
                (tuple fail
                  (add-data succeed (eval expression) data))))
            ; did we get the wrong exception class?
            ((/= actual-class expected-class)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-exception-class
                    (tuple expected-class expected-term
                      (: erlang get_stacktrace)) data))))
            ; did we get the wrong exception term?
            ((/= actual-term expected-term)
              (: erlang error
                (tuple fail
                  (add-data 'unexpected-exception-term
                    (tuple expected-class expected-term
                      (: erlang get_stacktrace)) data))))))))))

; XXX add implementation for this function
(defun assert-not-exception (expected-class expected-term expression)
  "
  This function check that the passeed expression does not raise the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.).
  "
  'true)

(defun assert-error (expected-term expression)
  "
  This function is a convenience function for assert-exception with an
  exception class of 'error.
  "
  (assert-exception 'error expected-term expression))

(defun assert-not-error (expected-term expression)
  "
  This function is a convenience function for assert-not-exception with an
  exception class of 'error.
  "
  (assert-not-exception 'error expected-term expression))

(defun assert-exit (expected-term expression)
  "
  This function is a convenience function for assert-exception with an
  exception class of 'exit.
  "
  (assert-exception 'exit expected-term expression))

(defun assert-not-exit (expected-term expression)
  "
  This function is a convenience function for assert-not-exception with an
  exception class of 'exit.
  "
  (assert-not-exception 'exit expected-term expression))

(defun assert-throw (expected-term expression)
  "
  This function is a convenience function for assert-exception with an
  exception class of 'throw.
  "
  (assert-exception 'throw expected-term expression))

(defun assert-not-throw (expected-term expression)
  "
  This function is a convenience function for assert-not-exception with an
  exception class of 'throw.
  "
  (assert-not-exception 'throw expected-term expression))

; XXX add implementation for this function
(defun assert-match (guard expression)
  ""
  'true)

; XXX add implementation for this function
(defun assert-not-match (guard expression)
  ""
  'true)