(defpackage :fiveam-asdf-asd
  (:use :common-lisp :asdf)
  )

(in-package :fiveam-asdf-asd)

(defsystem fiveam-asdf
  :long-description
  "System that defines a new system class FIVEAM-TESTER
that provides functionality for running tests using
FIVEAM and raising an error if the tests fail
\(useful for incorporation in a Jenkins or Hudson
build\)."
  :components ((:file "code"))
  )