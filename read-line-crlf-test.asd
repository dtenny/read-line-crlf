(in-package :cl-user)

(defpackage :read-line-crlf-test-asd
  (:use :cl :asdf))

(in-package :read-line-crlf-test-asd)

(defsystem :read-line-crlf-test
  :version "1.0.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for the :read-line-crlf package."
  :depends-on (:read-line-crlf :fiveam)
  :components ((:file "read-line-crlf-test")))
