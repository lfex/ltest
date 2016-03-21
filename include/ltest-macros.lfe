;; Include EUnit macros
(include-lib "eunit/include/eunit.hrl")

;;;===================================================================
;;; OTP 18 hacks
;;;===================================================================

(defmacro otp-18? () `(=:= "18" (erlang:system_info 'otp_release)))

(defmacro assertion-failed () `(if (otp-18?) 'assert 'assertion_failed))

(defmacro assert-equal-failed ()
  `(if (otp-18?) 'assertEqual 'assertEqual_failed))

(defmacro assert-not-equal-failed ()
  `(if (otp-18?) 'assertNotEqual 'assertNotEqual_failed))

(defmacro assert-exception-failed ()
  `(if (otp-18?) 'assertException 'assertException_failed))

(defmacro assert-match-failed ()
  `(if (otp-18?) 'assertMatch 'assertMatch_failed))


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


;;;===================================================================
;;; Test definition macros
;;;===================================================================

(defmacro deftest
  "Define a standard EUnit test."
  ([name . body]
   `(defun ,(list_to_atom (++ (to-unders name) "_test")) ()
      ,@body)))

(defmacro deftestgen
  "Define an EUnit test that uses test generators."
  ([name . body]
   `(defun ,(list_to_atom (++ (to-unders name) "_test_")) ()
      ,@body)))

(defmacro deftestskip
  "Define a standard EUnit test that will be skipped (not run)."
  ([name . body]
   `(defun ,(list_to_atom (++ (to-unders name) "_skip")) ()
      ,@body)))


;;;===================================================================
;;; Set-up and tear-down macros
;;;===================================================================

(defmacro defsetup (func-name)
  "Return a nullary function that calls `func-name/0`.

A simple wrapper macro for defining the set-up function in a fixture."
  `(lambda () (,func-name)))

(defmacro defteardown (func-name)
  "Return a unary function that calls `func-name/1` on its argument.

A simple wrapper macro for defining the tear-down function in a fixture."
  `(lambda (x) (,func-name x)))


;;;===================================================================
;;; Test case macros
;;;===================================================================

(defmacro deftestcase
  "This macro is for defining EUnit tests for use by fixtures which have
  particular naming convention needs."
  ([func-name args . rest]
   `(defun ,(list_to_atom (++ (to-unders func-name) "_test_case")) (,@args)
      (list
        ,@(lists:map
            (lambda (part)
              `(lambda () ,part))
            rest)))))

(defmacro deftestcases funcs
  "This macro expects one or more function *names* which have been defined
  using [[deftestcase/255]].

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

(defmacro is-exception
  "Evaluate `expression`, catching any exception and testing that it matches
`` `#(,expected-class ,expected-term) ``. If the match fails, or if no exception
is thrown, an informative exception will be generated.

[[is-error/2]], [[is-exit/2]] and [[is-throw/2]] are equivalent to using
[[is-exception/3]] with an `expected-class` of `'error`, `'exit`, or
`'throw`, respectively."
  ([expression]
   `(is-exception _ _ ,expression))
  ([expected-class expected-term expression]
   `(assertException ,expected-class ,expected-term ,expression)))

(defmacro is-not-exception
  "The inverse case of [[is-exception/3]], for convenience."
  ([expression]
   `(is-not-exception _ _ ,expression))
  ([expected-class expected-term expression]
   `(assertNotException ,expected-class ,expected-term ,expression)))

(defmacro is-error
  "Equivalent to [[is-exception/3]] with `'error` as `expected-class`."
  ([expression] `(is-error _ ,expression))
  ([error body] `(assertError ,error ,body)))

(defmacro is-not-error
  "The inverse case of [[is-error/2]], for convenience."
  ([expression]
   `(is-not-error _ ,expression))
  ([expected-term expression]
   `(is-not-exception 'error ,expected-term ,expression)))

(defmacro is-exit
  "Equivalent to [[is-exception/3]] with `'exit` as `expected-class`."
  ([expression]               `(is-exit _ ,expression))
  ([expected-term expression] `(assertExit ,expected-term ,expression)))

(defmacro is-not-exit
  "The inverse case of [[is-exit/2]], for convenience."
  ([expression]
   `(is-not-exit _ ,expression))
  ([expected-term expression]
   `(is-not-exception 'exit ,expected-term ,expression)))

(defmacro is-throw
  "Equivalent to [[is-exception/3]] with `'throw` as `expected-class`."
  ([expression]               `(is-throw _ ,expression))
  ([expected-term expression] `(assertThrow ,expected-term ,expression)))

(defmacro is-not-throw
  "The inverse case of [[is-throw/2]], for convenience."
  ([expression]
   `(is-not-throw _ ,expression))
  ([expected-term expression]
   `(is-not-exception 'throw ,expected-term ,expression)))


;;;===================================================================
;;; Test object macros
;;;===================================================================

(defmacro is* (bool-expression)
  "Return a test object that wraps [[is/1]]."
  `(_assert ,bool-expression))

(defmacro is-not* (bool-expression)
  "Return a test object that wraps [[is-not/2]]."
  `(_assertNot ,bool-expression))

(defmacro is-match* (guard expression)
  "Return a test object that wraps [[is-match/2]]."
  `(_assertMatch ,guard ,expression))

(defmacro is-not-match* (guard expression)
  "The inverse case of [[is-match*/2]], for convenience."
  `(_assertMatch ,guard ,expression))

(defmacro is-equal* (value expression)
  "Return a test object to assert `expression` evaluates to `value`."
  `(_assertEqual ,value ,expression))

(defmacro is-not-equal* (value expression)
  "Return a test object to assert `expression` evaluates to `value`."
  `(_assertNotEqual ,value ,expression))

(defmacro is-exception*
  "Return a test object that wraps [[is-exception/3]]."
  ([expression]
   `(is-exception* _ _ ,expression))
  ([expected-class expected-term expression]
   `(_assertException ,expected-class ,expected-term ,expression)))

(defmacro is-not-exception*
  "The inverse case of [[is-exception*/3]], for convenience."
  ([expression]
   `(is-not-exception* _ _ ,expression))
  ([expected-class expected-term expression]
   `(_assertNotException ,expected-class ,expected-term ,expression)))

(defmacro is-error*
  "Return a test object that wraps [[is-error/2]]."
  ([expression] `(is-error* _ ,expression))
  ([error body] `(_assertError ,error ,body)))

(defmacro is-not-error*
  "Return a test object that wraps [[is-not-error/2]]."
  ([expression] `(is-not-error* _ ,expression))
  ([error body] `(_test (is-not-error ,error ,body))))

(defmacro is-exit*
  "Return a test object that wraps [[is-exit/2]]"
  ([expression]               `(is-exit* _ ,expression))
  ([expected-term expression] `(_assertExit ,expected-term ,expression)))

(defmacro is-not-exit* (expected-term expression)
  "Return a test object that wraps [[is-not-exit/2]]."
  ([expression]         `(is-not-exit* _ ,expression))
  ([expected-term body] `(_test (is-not-exit ,expected-term ,expression))))

(defmacro is-throw* (expected-term expression)
  "Return a test object that wraps [[is-throw/2]]."
  ([expression]         `(is-throw* _ ,expression))
  ([expected-term body] `(_assertThrow ,expected-term ,expression)))

(defmacro is-not-throw* (expected-term expression)
  "Return a test object that wraps [[is-not-throw/2]]."
  ([expression]
   `(is-not-throw* _ ,expression))
  ([expected-term body]
   `(_test (is-not-throw 'throw ,expected-term ,expression))))
