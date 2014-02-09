(include-lib "eunit/include/eunit.hrl")


(defmacro returntype (name cmds)
  `(defun ,name () ',cmds))

(defmacro deftest-1 (name body)
  `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
     ,body))

(defmacro deftest-2 (name body)
  `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
     (: lists map
      (lambda (expr)
        (funcall expr))
      ,body)))

(defmacro deftest-3 (name body)
  `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
     'an-atom))

(defmacro deftest-4 arg
  "This one looks like the winner."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
       ,@body)))

(defmacro deftest-5 arg
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
       (: lists map
         (lambda (expr)
           (funcall expr))
         ',body))))

(defmacro is (bool-expression)
  "This function takes an expression that returns a boolean value. If the
  expression does not evaluate as a truth value, an error is returned."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "This function takes an expression that returns a boolean value. If the
  expression does not evaluate as false value, an error is returned."
  `(assertNot ,bool-expression))

(defmacro is-equal (value expression)
  "This function checks the equality between a passed value and a quoated
  expression."
  `(assertEqual ,value ,expression))

(defmacro is-not-equal (value expression)
  "This function checks the inequality between an expected value and a passed
  expression."
  `(assertNotEqual ,value ,expression))

(defmacro is-exception (expected-class expected-term expression)
  "This function check that the passeed expression raises the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(assertException ,expected-class ,expected-term ,expression))

(defmacro is-not-exception (expected-class expected-term expression)
  "This function check that the passeed expression does not raise the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(not (is-exception ,expected-class ,expected-term ,expression)))

(defmacro is-error (error body)
  "This function is a convenience function for is-exception with an
  exception class of 'error."
  `(assertError ,error ,body))

(defmacro is-not-error (expected-term expression)
  "This function is a convenience function for is-not-exception with an
  exception class of 'error."
  `(is-not-exception 'error ,expected-term ,expression))

(defmacro is-exit (expected-term expression)
  "This function is a convenience function for is-exception with an
  exception class of 'exit."
  `(assertExit ,expected-term ,expression))

(defmacro is-not-exit (expected-term expression)
  "This function is a convenience function for is-not-exception with an
  exception class of 'exit."
  `(is-not-exception 'exit ,expected-term ,expression))

(defmacro is-throw (expected-term expression)
  "This function is a convenience function for is-exception with an
  exception class of 'throw."
  `(assertThrow ,expected-term ,expression))

(defmacro is-not-throw (expected-term expression)
  "This function is a convenience function for is-not-exception with an
  exception class of 'throw."
  `(is-not-exception 'throw ,expected-term ,expression))

(defmacro is-match (guard expression)
  ""
  `(assertMatch ,guard ,expression))

(defmacro is-not-match (guard expression)
  ""
  `(not (is-match ,guard ,expression)))
