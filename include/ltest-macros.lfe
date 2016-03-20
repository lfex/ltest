;; Include EUnit macros
(include-lib "eunit/include/eunit.hrl")


;;;===================================================================
;;; Helper functions
;;;===================================================================

(eval-when-compile
  (defun to-unders (atm)
    (re:replace (atom_to_list atm) "-" "_" '(#(return list) global)))
  (defun list-body
    ([body] (when (is_list body))
     body)
    ([body]
     (list body)))
  ;; end of eval-when-compile
  )

(defmacro deftest arg
  "This macro is for defining standard EUnit tests."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) "_test")) ()
       ,@body)))

(defmacro deftestgen arg
  "This macro is for defining EUnit tests that use test generators."
  (let ((name (to-unders (car arg)))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ name "_test_")) ()
       ,@body)))

(defmacro deftestskip arg
  "This macro is for defining standard EUnit test that will be skipped (not
  run)."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) "_skip")) ()
       ,@body)))

(defmacro defsetup (func-name)
  "A simple wrapper macro for defining the set-up function in a fixture."
  `(lambda () (,func-name)))

(defmacro defteardown (func-name)
  "A simple wrapper macro for defining the tear-down function in a fixture."
  `(lambda (x) (,func-name x)))

(defmacro deftestcase body
  "This macro is for defining EUnit tests for use by fixtures which have
  particular naming convention needs."
  (let ((func-name (car body))
        (args (cadr body))
        (rest (cddr body)))
  `(defun ,(list_to_atom (++ (to-unders func-name) "_test_case")) (,@args)
    (list
      ,@(lists:map
        (lambda (part)
          `(lambda () ,part))
        rest)))))

(defmacro deftestcases funcs
  "This macro expects one or more function *names* which have been defined
  using (deftestcase ...).

  Note that this macro is not composable with (deftestcase ...); you must
  define the test case and then only pass the test case name to this
  macro."
  (cond ((> (length funcs) 1)
         `(list
           ,@(lists:map
              (lambda (func-name)
                `(lambda (x)
                   (,(list_to_atom (++ (to-unders func-name) "_test_case")) x)))
              funcs)))
        ('true
         `(lambda (x)
            (,(list_to_atom (++ (to-unders (car funcs)) "_test_case")) x)))))


;;;===================================================================
;;; Assertion macros
;;;===================================================================

(defmacro is (bool-expression)
  "Assert `bool-expression` evaluates to `'true`."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "Assert `bool-expression` evaluates to `'false`."
  `(assertNot ,bool-expression))

(defmacro is-match (guard expression)
  "Assert `guard` matches `expression`.

The main reason to use [[is-match/2]], instead of matching with `=`,
is that it produces more detailed error messages."
  `(assertMatch ,guard ,expression))

(defmacro is-not-match (guard expression)
  "The inverse case of [[is-match/2]], for convenience."
  `(assertNotMatch ,guard ,expression))

(defmacro is-equal (value expression)
  "Assert `expression` evaluates to `value`."
  `(assertEqual ,value ,expression))

(defmacro is-not-equal (value expression)
  "The inverse case of [[is-equal/2]], for convenience."
  `(assertNotEqual ,value ,expression))

(defmacro is-exception (expected-class expected-term expression)
  "Evaluate `expression`, catching any exception and testing that it matches
`` `#(,expected-class ,expected-term) ``. If the match fails, or if no exception
is thrown, an informative exception will be generated.

[[is-error/2]], [[is-exit/2]] and [[is-throw/2]] are equivalent to using
[[is-exception/3]] with an `expected-class` of `'error`, `'exit`, or
`'throw`, respectively."
  `(assertException ,expected-class ,expected-term ,expression))

(defmacro is-not-exception (expected-class expected-term expression)
  "The inverse case of [[is-exception/3]], for convenience."
  `(not (is-exception ,expected-class ,expected-term ,expression)))

(defmacro is-error (error body)
  "Equivalent to [[is-exception/3]] with `'error` as `expected-class`."
  `(assertError ,error ,body))

(defmacro is-not-error (expected-term expression)
  "The inverse case of [[is-error/2]], for convenience."
  `(is-not-exception 'error ,expected-term ,expression))

(defmacro is-exit (expected-term expression)
  "Equivalent to [[is-exception/3]] with `'exit` as `expected-class`."
  `(assertExit ,expected-term ,expression))

(defmacro is-not-exit (expected-term expression)
  "The inverse case of [[is-exit/2]], for convenience."
  `(is-not-exception 'exit ,expected-term ,expression))

(defmacro is-throw (expected-term expression)
  "Equivalent to [[is-exception/3]] with `'throw` as `expected-class`."
  `(assertThrow ,expected-term ,expression))

(defmacro is-not-throw (expected-term expression)
  "The inverse case of [[is-throw/2]], for convenience."
  `(is-not-exception 'throw ,expected-term ,expression))
