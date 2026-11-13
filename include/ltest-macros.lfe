;; Include EUnit macros
(include-lib "eunit/include/eunit.hrl")

;;;===================================================================
;;; OTP 18 hacks
;;;===================================================================

(defmacro otp>=18? ()
  "Return true when the OTP release is 18 or newer; return false if it
   cannot be parsed."
  `(try
     (>= (list_to_integer (erlang:system_info 'otp_release)) 18)
     (catch ((tuple _ _ _) 'false))))

(defmacro assertion-failed ()
  "Return the version-specific EUnit assertion failure tag."
  `(if (otp>=18?) 'assert 'assertion_failed))

(defmacro assert-equal-failed ()
  "Return the version-specific EUnit equality failure tag."
  `(if (otp>=18?) 'assertEqual 'assertEqual_failed))

(defmacro assert-not-equal-failed ()
  "Return the version-specific EUnit inequality failure tag."
  `(if (otp>=18?) 'assertNotEqual 'assertNotEqual_failed))

(defmacro assert-exception-failed ()
  "Return the version-specific EUnit exception failure tag."
  `(if (otp>=18?) 'assertException 'assertException_failed))

(defmacro assert-match-failed ()
  "Return the version-specific EUnit match failure tag."
  `(if (otp>=18?) 'assertMatch 'assertMatch_failed))

;;;===================================================================
;;; Helper functions
;;;===================================================================

(eval-when-compile

  (defun to-unders (atm)
    (re:replace (atom_to_list atm) "-" "_" '(#(return list) global)))

  ;; XXX this is no longer used anywhere, right? remove it!
  (defun list-body
    ((body) (when (is_list body))
     body)
    ((body)
     (list body)))

  ) ; end of eval-when-compile

;;;===================================================================
;;; Test definition macros
;;;===================================================================

(defmacro deftest
  "Define a nullary EUnit test named NAME_test from the supplied body
   forms."
  ((cons name body)
   (let ((name_test (list_to_atom (++ (to-unders name) "_test"))))
     `(progn (defun ,name_test () ,@body)
             (extend-module () ((export (,name_test 0))))))))

(defmacro deftestgen
  "Define a nullary EUnit generator named NAME_test_ from the supplied
   body forms."
  ((cons name body)
   (let ((name_test_ (list_to_atom (++ (to-unders name) "_test_"))))
     `(progn (defun ,name_test_ () ,@body)
             (extend-module () ((export (,name_test_ 0))))))))

(defmacro deftestskip
  "Define a nullary NAME_skip function; ltest lists exported matches
   without executing the body."
  ((cons name body)
   `(defun ,(list_to_atom (++ (to-unders name) "_skip")) ()
      ,@body)))

;;;===================================================================
;;; Set-up and tear-down macros
;;;===================================================================

(defmacro defsetup (func-name)
  "Return a zero-argument fixture setup function that calls FUNC-NAME/0."
  `(lambda () (,func-name)))

(defmacro defteardown (func-name)
  "Return a one-argument fixture teardown function that calls
   FUNC-NAME/1."
  `(lambda (x) (,func-name x)))


;;;===================================================================
;;; Test case macros
;;;===================================================================

(eval-when-compile
  ;; Return true if we have (tuple "name"...) or #("Name"...)
  (defun is-named-tuple
    ((t) (when (is_tuple t))
      (io_lib:printable_list (element 1 t)))
    ((t) (when (is_list t))
      (andalso (== 'tuple (hd t))
               (io_lib:printable_list (cadr t))))
    ((other) 'false))

  ;; Return (tuple "Name" lambda() ...) from (tuple "Name" ...)
  (defun mk-named-tuple
    ((t) (when (is_tuple t))
      `(tuple ,(element 1 t)
              (lambda ()
                ,(list_to_tuple (tl (tuple_to_list t))))))
    ((t) (when (is_list t))
      `(tuple ,(cadr t)
              (lambda ()
                ,(list_to_tuple (cdr t)))))))

(defmacro deftestcase
  "Define a fixture test-case function; printable named tuples become
   named test objects and other forms become nullary tests."
  ((cons func-name (cons args rest))
   `(defun ,(list_to_atom (++ (to-unders func-name) "_test_case")) (,@args)
      (list
        ,@(lists:map
            (lambda (part)
              (if (is-named-tuple part)
                ;; Make a named tuple if part is a tuple and its
                ;; 1st element is printable
                (mk-named-tuple part)
                ;; Otherwise just make a lamdba
                `(lambda () ,part)))
            rest)))))

(defmacro deftestcases funcs
  "Build a fixture generator from one or more previously defined
   deftestcase functions."
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

(defmacro is (bool-expression) "Assert that the expression evaluates to true."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "Assert that the expression evaluates to false.
   Assert that the expression evaluates to false."
  `(assertNot ,bool-expression))

(defmacro is-match (guard expression)
  "Assert that the expression matches GUARD and report an EUnit match
   diagnostic on failure."
  `(assertMatch ,guard ,expression))

(defmacro is-not-match (guard expression)
  "Assert that the expression does not match GUARD.
   Assert that the expression does not match GUARD."
  `(assertNotMatch ,guard ,expression))

(defmacro is-equal (value expression)
  "Assert that EXPRESSION evaluates to VALUE.
   Assert that EXPRESSION evaluates to VALUE."
  `(assertEqual ,value ,expression))

(defmacro is-not-equal (value expression)
  "Assert that EXPRESSION does not evaluate to VALUE.
   Assert that EXPRESSION does not evaluate to VALUE."
  `(assertNotEqual ,value ,expression))

(defmacro is-exception
  "Assert that EXPRESSION raises the requested exception class and term;
   one argument accepts any class and term."
  (`(,expression)
   `(is-exception _ _ ,expression))
  (`(,expected-class ,expected-term ,expression)
   `(assertException ,expected-class ,expected-term ,expression)))

(defmacro is-not-exception
  "With one argument, assert that no exception is raised; with explicit
   class and term, reject only that pair."
  (`(,expression)
   `(is-not-exception _ _ ,expression))
  (`(,expected-class ,expected-term ,expression)
   `(assertNotException ,expected-class ,expected-term ,expression)))

(defmacro is-error
  "Assert that EXPRESSION raises an error, optionally matching its term."
  (`(,expression) `(is-error _ ,expression))
  (`(,error ,body) `(assertError ,error ,body)))

(defmacro is-not-error
  "Reject the selected error term; other exception classes may pass."
  (`(,expression)
   `(is-not-error _ ,expression))
  (`(,expected-term ,expression)
   `(is-not-exception 'error ,expected-term ,expression)))

(defmacro is-exit
  "Assert that EXPRESSION exits, optionally matching its term."
  (`(,expression) `(is-exit _ ,expression))
  (`(,expected-term ,expression) `(assertExit ,expected-term ,expression)))

(defmacro is-not-exit
  "Reject the selected exit term; other exception classes may pass."
  (`(,expression)
   `(is-not-exit _ ,expression))
  (`(,expected-term ,expression)
   `(is-not-exception 'exit ,expected-term ,expression)))

(defmacro is-throw
  "Assert that EXPRESSION throws, optionally matching its term."
  (`(,expression) `(is-throw _ ,expression))
  (`(,expected-term ,expression) `(assertThrow ,expected-term ,expression)))

(defmacro is-not-throw
  "Reject the selected throw term; other exception classes may pass."
  (`(,expression)
   `(is-not-throw _ ,expression))
  (`(,expected-term ,expression)
   `(is-not-exception 'throw ,expected-term ,expression)))

;;;===================================================================
;;; Test object macros
;;;===================================================================

;; Based on clojure.walk
(eval-when-compile
  ;; FIXME: walk more data structures
  (defun walk
    ((inner outer form) (when (is_list form))
     (funcall outer (lists:map inner form)))
    ((_inner outer form) (funcall outer form)))

  (defun postwalk (f form)
    ;; N.B. Due to implementation details, we can't use
    ;;          (clj:partial #'postwalk/2 f)
    (walk (lambda (inner-form)
            (postwalk f inner-form))
          f
          form))

  (defun postwalk-replace (proplist form)
    (postwalk (lambda (|-X-|)
                (proplists:get_value |-X-| proplist |-X-|))
              form))

  (defun apply-template (arglist expr values)
    (orelse (is_list arglist)
            (error 'badarg (list arglist expr values)))
    (orelse (lists:all #'is_atom/1 arglist))
    (postwalk-replace (lists:zip arglist values) expr))

  ;; Based on #'clojure.template/do-template
  (defmacro do-template
    (`(,arglist ,expr . ,values)
     (let ((|-LEN-| (length arglist)))
       `(list ,@(lists:map (lambda (|-A-|) (apply-template arglist expr |-A-|))
                           (clj:partition |-LEN-| values))))))

  ) ; end eval-when-compile

(defmacro is* (bool-expression)
  "Construct a lazy EUnit test object that asserts the expression is
   true."
  `(_assert ,bool-expression))

;; Based on #'clojure.test/are
(defmacro are*
  "Substitute each complete row of values into the expression
   immediately; this does not return delayed test objects."
  ('(() ,expr) `(is* 'true))
  (`(,arglist ,expr . ,args)
   `(do-template ,arglist (is ,expr) ,@args))
  (_ (error 'badarg (list* arglist expr args))))

(defmacro is-not* (bool-expression)
  "Construct a lazy EUnit test object that asserts the expression is
   false."
  `(_assertNot ,bool-expression))

(defmacro is-match* (guard expression)
  "Construct a lazy EUnit test object that asserts the expression matches
   GUARD."
  `(_assertMatch ,guard ,expression))

(defmacro is-not-match* (guard expression)
  "Construct a lazy EUnit test object that asserts the expression does
   not match GUARD."
  `(_assertNotMatch ,guard ,expression))

(defmacro is-equal* (value expression)
  "Construct a lazy EUnit test object that asserts EXPRESSION evaluates
   to VALUE."
  `(_assertEqual ,value ,expression))

(defmacro is-not-equal* (value expression)
  "Construct a lazy EUnit test object that asserts EXPRESSION does not
   evaluate to VALUE."
  `(_assertNotEqual ,value ,expression))

(defmacro is-exception*
  "Construct a lazy exception assertion object; the three-argument form
   checks class and term."
  (`(,expression)
   `(is-exception* _ _ ,expression))
  (`(,expected-class ,expected-term ,expression)
   `(_assertException ,expected-class ,expected-term ,expression)))

(defmacro is-not-exception*
  "With one argument, construct an object asserting no exception; with
   explicit class and term, reject only that pair."
  (`(,expression)
   `(is-not-exception* _ _ ,expression))
  (`(,expected-class ,expected-term ,expression)
   `(_assertNotException ,expected-class ,expected-term ,expression)))

(defmacro is-error*
  "Construct a lazy EUnit object that asserts an error, optionally
   matching its term."
  (`(,expression) `(is-error* _ ,expression))
  (`(,error ,body) `(_assertError ,error ,body)))

(defmacro is-not-error*
  "Reject the selected error term when EUnit evaluates the lazy object;
   other classes may pass."
  (`(,expression) `(is-not-error* _ ,expression))
  (`(,error ,body) `(_test (is-not-error ,error ,body))))

(defmacro is-exit*
  "Construct a lazy EUnit object that asserts an exit, optionally
   matching its term."
  (`(,expression) `(is-exit* _ ,expression))
  (`(,expected-term ,expression) `(_assertExit ,expected-term ,expression)))

(defmacro is-not-exit*
  "Reject the selected exit term when EUnit evaluates the lazy object;
   other classes may pass."
  (`(,expression) `(is-not-exit* _ ,expression))
  (`(,expected-term ,body) `(_test (is-not-exit ,expected-term ,body))))

(defmacro is-throw*
  "Construct a lazy EUnit object that asserts a throw, optionally
   matching its term."
  (`(,expression) `(is-throw* _ ,expression))
  (`(,expected-term ,body) `(_assertThrow ,expected-term ,body)))

(defmacro is-not-throw*
  "Reject the selected throw term when EUnit evaluates the lazy object;
   other classes may pass."
  (`(,expression)
   `(is-not-throw* _ ,expression))
  (`(,expected-term ,body)
   `(_test (is-not-throw ,expected-term ,body))))

(defun --loaded-ltest-macros-- ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
