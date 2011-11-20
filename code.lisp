(in-package :asdf)

(defclass fiveam-tester-system (system)
  ((test-names
    :initarg :otest-names
    :reader test-names
    :documentation "An ALIST of symbol and
package designators, used to indicate which
FIVEAM tests should be run when the TEST-OP
is invoked on this system."
    ))
  )

(defmethod perform ((op test-op) (sys fiveam-tester-system))
  (with-slots (test-names) sys
    (let* ((test-syms
            (loop for (test-name . package-name) in test-names
                  for package = (or (find-package package-name)
                                    (error "Unable to find package ~a" package-name))
                  for test-sym = (intern
                                  (etypecase test-name
                                    (string test-name)
                                    (symbol (symbol-name test-name)))
                                  package)
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




