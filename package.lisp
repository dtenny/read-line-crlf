(in-package :cl-user)

(defpackage :read-line-crlf
  (:use :cl)
  (:export :read-line-crlf)
  (:documentation
   "Replacement for CL:READ-LINE that respects LF, CRLF, and CR line separators."))

  
