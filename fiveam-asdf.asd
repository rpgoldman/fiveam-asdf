;;;---------------------------------------------------------------------------
;;; Copyright (c) 2012-2019 Smart Information Flow Technologies, d/b/a SIFT, LLC and
;;; Robert P. Goldman
;;; All rights reserved.
;;;
;;; The developers make this software available according to the terms of the
;;; Lisp Lesser GNU Public License (LLGPL).
;;; See http://opensource.franz.com/preamble.html
;;;    
;;;---------------------------------------------------------------------------
;;;
;;; This system provides functionality to support integrating FiveAM tests with
;;; an ASDF system definition in such a way that invocation of the
;;; ASDF:TEST-SYSTEM function will cause the FiveAM tests to run.  An error will
;;; be raised if there are test failures.
;;;
;;;---------------------------------------------------------------------------



(defsystem "fiveam-asdf"
  :long-description
  "System that defines a new system class FIVEAM-TESTER
that provides functionality for running tests using
FIVEAM and raising an error if the tests fail
\(useful for incorporation in a Jenkins or Hudson
build\)."
  :description "Library to integrate FiveAM testing with ASDF TEST-OP and TEST-SYSTEM"
  :depends-on (:asdf)
  :components (
	       (:file "code"))
  :author "Robert P. Goldman and John Maraist"
  :version "2.0"
  :license "Lisp LGPL"
  )
