(in-package :cl-user)

(defpackage :read-line-crlf-asd
  (:use :cl :asdf))

(in-package :read-line-crlf-asd)

(defsystem :read-line-crlf
  :version "1.0.1"
  :license "MIT"
  :author "Dave Tenny"
  :description "READ-LINE replacement that respects LF, CR, and CRLF line terminations."
  :bug-tracker "https://github.com/dtenny/read-line-crlf/issues"
  :source-control (:git "https://github.com/dtenny/read-line-crlf")
  :serial t
  :components ((:file "package")
               (:file "read-line-crlf")))
