(uiop:define-package #:fiveam-asdf
  (:use #:uiop #:asdf #:cl)
  (:export #:fiveam-tester-system #:package-inferred-fiveam-tester-system))
(in-package #:fiveam-asdf)

(defclass fiveam-tester-system (system)
  ((test-names
    :initarg :test-names
    :reader test-names
    :documentation "A list of test designators, each of which is either a symbol designator \
or a cons of (SYMBOL-NAME . PACKAGE-DESIGNATOR).

Bare symbols will be interned in the package designated by the TEST-PACKAGE slot, which must be bound if \
any are to be interned this way.

The symbol designators, SYMBOL-NAMEs, and PACKAGE-DESIGNATORs may each be any of: a keyword, a string or an \
uninterned symbol.")
   (test-package
    :initarg :default-test-package
    :initarg :test-package
    :documentation "A package designator for the TEST-NAMES which don't have explicit packages listed.

If all the tests are in one package, you can just have a list of symbol designators (strings or keywords) in \
test-names, and get the package name from here.")
   (num-checks
    :initarg :num-checks
    :reader num-checks
    :type (or null (integer 0))
    :initform nil
    :documentation "Expected number of tests to be run when you invoke test-op on this system.

If supplied and non-NIL, then when running the test-op, we will fail if the actual number of checks run does \
not match the expected number expected number. See the FiveAM manual for the definition of a check and how \
they are counted.")))

(defclass package-inferred-fiveam-tester-system (package-inferred-system fiveam-tester-system)
  ())

(define-condition fiveam-asdf-failure (error)
  ((failed-asdf-component
    :initarg :failed-asdf-component
    :reader failed-asdf-component))
  (:documentation "Superclass of error conditions that indicate that an ASDF test-op has failed."))

(define-condition fiveam-test-fail (fiveam-asdf-failure)
  ((failed
    :initarg :failed
    :reader failed
    :documentation "A list of failed tests"))
  (:report (lambda (x s)
              (format s "Tests on system ~a failed: ~{~t~a~%~}"
                      (component-name (failed-asdf-component x))
                      (failed x))))
  (:documentation "Thrown when a FiveAM test fails when testing a `fiveam-tester-system'"))

(define-condition fiveam-wrong-number-of-checks (fiveam-asdf-failure)
  ((expected-num-checks
    :initarg :expected-num-checks
    :reader expected-num-checks)
   (actual-num-checks
    :initarg :actual-num-checks
    :reader actual-num-checks))
  (:report (lambda (x s)
              (format s "Unexpected number of tests on system ~a: Expected ~d got ~d."
                      (component-name (failed-asdf-component x))
                      (expected-num-checks x)
                      (actual-num-checks x))))
  (:documentation "Thrown when a FiveAM test suite has no failed tests, but the number of checks run does \
not match the expected number."))

(defgeneric test-package (x)
  (:method ((x fiveam-tester-system))
    (if (slot-boundp x 'test-package)
        (slot-value x 'test-package)
        (error "If package is not specified with each test-name, system's TEST-PACKAGE slot must be set."))))

(defun test-designator-name (test-designator)
  (etypecase test-designator
    (symbol (symbol-name test-designator))
    (string test-designator)
    (cons (test-designator-name (car test-designator)))))

(defun test-designator-package (test-designator tester-system)
  (etypecase test-designator
    ((or symbol string) (test-package tester-system))
    (cons (or (find-package (cdr test-designator))
              (error "Unable to find package ~a" (cdr test-designator))))))

(defun test-designator-symbol (test-designator tester-system)
  (intern (test-designator-name test-designator)
          (test-designator-package test-designator tester-system)))

(defun test-syms (tester-system)
  (loop for test-designator in (test-names tester-system)
        collect (test-designator-symbol test-designator tester-system)))

(defun run-tests (tester-system)
  (loop with runner = (intern (symbol-name '#:run) '#:fiveam)
        for test in (test-syms tester-system)
        appending (funcall runner test)))

(defun explain-results (results)
  (funcall (intern (symbol-name '#:run) '#:fiveam)
           results))

(defun verify-num-checks (results tester-system)
  "Throw an error if TESTER-SYSTEM specifies aan expected number of checks to which RESULTS does not conform."
  (if-let ((expected (num-checks tester-system))
           (actual (length results)))
    (unless (= actual expected)
      (error 'fiveam-wrong-number-of-checks
             :failed-asdf-component tester-system
             :actual-num-checks actual
             :expected-num-checks expected))))

(defun results-status (results)
  "Returns (values SUCCESSP FAILED-CHECKS)"
  (funcall (intern (symbol-name '#:results-status) '#:fiveam)
           results))

(defun verify-success (results tester-system)
  (multiple-value-bind (success failures) (results-status results)
    (unless success
      (error 'fiveam-test-fail
             :failed-asdf-component tester-system
             :failed failures))))

(defmethod perform ((op test-op) (sys fiveam-tester-system))
  (let* ((results (run-tests sys)))
    (explain-results results)
    (verify-num-checks results sys)
    (verify-success results sys)))

(defmethod component-depends-on ((op load-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(defmethod component-depends-on ((op compile-op) (sys fiveam-tester-system))
  (cons '(load-op "fiveam") (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(fiveam-tester-system package-inferred-fiveam-tester-system)
          '#:asdf)
  (export '(fiveam-tester-system package-inferred-fiveam-tester-system)
          '#:asdf))
