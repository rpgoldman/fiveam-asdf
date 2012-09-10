;;;---------------------------------------------------------------------------
;;; Copyright (c) 2012 Smart Information Flow Technologies, d/b/a SIFT, LLC and
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
  :version "1.0"
  )