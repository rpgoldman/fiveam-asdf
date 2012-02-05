(in-package :asdf)

(defclass fiveam-tester-system (system)
  ((test-names
    :initarg :test-names
    :reader test-names
    :documentation "A list whose elments are either
cons cells of symbol and package designators or
simply a symbol designator.
  In the latter case, the symbols will be interned
in the package designated by the TEST-PACKAGE slot,
which must be bound."
    )
   (test-package
    :initarg :default-test-package
    :documentation "If all the tests are in one
package, you can just have a list of test names
in test-names, and get the package name from here."
  )))

(defmethod test-package ((x fiveam-tester-system))
  (if (slot-boundp x 'test-package)
      (slot-value x 'test-package)
      (error "If package is not specified with each test-name, system's TEST-PACKAGE slot must be set.")))

(defmethod perform ((op test-op) (sys fiveam-tester-system))
  (with-slots (test-names) sys
    (let* ((test-syms
            (loop for x in test-names
                  with test-name and package-name and test-sym
                  if (symbolp x)
                  do (setf test-name x
                           package-name (test-package sys))
                  else
                     do (assert (and (consp x)
                                  (or (symbolp (car x)) (stringp (car x)))
                                  (or (symbolp (cdr x)) (stringp (cdr x)))))
                        (setf test-name (car x) package-name (cdr x))
                  do (setf package (or (find-package package-name)
                                       (error "Unable to find package ~a" package-name)))
                     (setf test-sym
                           (intern
                            (etypecase test-name
                              (string test-name)
                              (symbol (symbol-name test-name)))
                            package))
                  collect test-sym))
           (runner (intern (symbol-name '#:run) :fiveam))
           (tester (intern (symbol-name '#:results-status) :fiveam))
           (explainer (intern (symbol-name '#:explain!) :fiveam))
           (results (loop for test in test-syms
                          appending (funcall runner test))))
      (funcall explainer results)
      (unless (funcall tester results)
        (error "Tests on system ~a failed: ~a" (component-name sys) results)))))

(defmethod component-depends-on ((op load-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(defmethod component-depends-on ((op compile-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'fiveam-tester-system :asdf))




