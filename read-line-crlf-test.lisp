(in-package :cl-user)

(defpackage :read-line-crlf-test
  (:use :cl :read-line-crlf :fiveam)
  (:export #:run-tests)
  (:documentation "Tests for the :read-line-crlf package."))

(in-package :read-line-crlf-test)

(def-suite test-suite :description ":read-line-crlf tests")
(in-suite test-suite)

(defvar *strings*
  (list
   "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
   " "
   ;; more than 1x buffer size
   "Morbi suscipit tincidunt magna sed venenatis. Duis ac turpis et quam rhoncus blandit ac eget tortor. Integer vehicula ipsum ut ligula convallis rhoncus."
   "In sodales justo a nisi vestibulum luctus. Morbi rhoncus, sapien quis gravida maximus, "
   ""
   "odio ex pretium neque, id vehicula sapien magna sit amet velit."
   ""
   ""
   ;; More than 3x buffer size
   "Suspendisse congue, nisl eu volutpat dictum, libero purus mattis lectus, id commodo augue nisl sed erat. Pellentesque quis nisl erat. Nullam mollis elit varius odio accumsan lobortis. Quisque purus orci, dignissim vel lacinia mattis, dictum at orci. Proin velit dolor, scelerisque at ligula ultrices, vehicula scelerisque justo. Pellentesque aliquam nulla eget risus aliquet mattis. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nulla laoreet in nunc et varius. Suspendisse varius id erat sit amet venenatis. Duis aliquet posuere massa non tincidunt. Pellentesque faucibus, mi eu porttitor rhoncus, turpis augue faucibus metus, nec molestie erat nunc ut ex. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Proin pulvinar risus ut risus finibus facilisis. Donec vel egestas urna."
   ""
   ""
   ""
   "The End."
   ))

(defvar *double-strings*
  (loop for string in *strings*
        collect string
        collect ""))

(defun big-string (eol)
  (with-output-to-string (stream)
             (loop for string in *strings*
                   do (princ string stream)
                      (princ eol stream))))

(defmacro with-test-stream ((stream eol) &body body)
  "Create a test input stream with lines separated by EOL, a string.
Execute BODY with STREAM bound to the input stream."
  `(with-input-from-string (,stream (big-string ,eol))
     ,@body))
                   
(defvar *LF* (princ-to-string #\linefeed))
(defvar *CR* (princ-to-string #\return))
(defvar *CRLF* (concatenate 'string *CR* *LF*))
(defvar *LFLF* (concatenate 'string *LF* *LF*))
(defvar *CRCR* (concatenate 'string *CR* *CR*))
(defvar *CRCRLF* (concatenate 'string *CR* *CR* *LF*))

(defun read-lines (stream)
  "Return a list of strings as read from STREAM with read-line-crlf" 
  (loop as str = (read-line-crlf stream nil nil nil)
        while str
        collect str))

(test test-eol
  (signals end-of-file
    (with-input-from-string (stream "")
      (read-line-crlf stream)))        

  (is (eql 'foo
           (with-input-from-string (stream "")
             (read-line-crlf stream nil 'foo))))

  (is (equalp '("abc")
              (with-input-from-string (stream "abc")
                (read-lines stream))))
  (is (equalp NIL
              (with-input-from-string (stream "")
                (read-lines stream))))
  (is (equalp '("abc" "def")
              (with-input-from-string (stream (concatenate 'string "abc" *LF* "def"))
                (read-lines stream))))
  (is (equalp '("abc" "def")
              (with-input-from-string (stream (concatenate 'string "abc" *CR* "def"))
                (read-lines stream))))
  (is (equalp '("abc" "def")
              (with-input-from-string (stream (concatenate 'string "abc" *CRLF* "def"))
                (read-lines stream)))))

(test test-lf
  (is (equalp *strings* 
              (with-test-stream (stream *LF*)
                (read-lines stream)))))
(test test-cr
  (is (equalp *strings* 
              (with-test-stream (stream *CR*)
                (read-lines stream)))))
(test test-crlf
  (is (equalp *strings* 
              (with-test-stream (stream *CRLF*) ;one eol, not two
                (read-lines stream)))))
(test double-eol
  (is (equalp *double-strings* 
              (with-test-stream (stream *CRCR*)
                (read-lines stream))))
  (is (equalp *double-strings* 
              (with-test-stream (stream *LFLF*)
                (read-lines stream))))
  (is (equalp *double-strings* 
              (with-test-stream (stream *CRCRLF*) ; reads as two eols, not three
                (read-lines stream)))))

(test buffer
  (with-input-from-string (stream "abcdef")
    (let ((buffer (make-string 2)))
      (is (equalp "abcdef" (read-line-crlf stream nil nil nil buffer)))
      (is (equalp buffer "ab"))))

  (flet ((read-lines (stream)
           (loop as str = (read-line-crlf stream nil nil nil (make-string 1))
                 while str
                 collect str)))
    (is (equalp *strings* 
                (with-test-stream (stream *LF*)
                  (read-lines stream)))))
  (flet ((read-lines (stream)
           (loop as str = (read-line-crlf stream nil nil nil (make-string 2048))
                 while str
                 collect str)))
    (is (equalp *strings* 
                (with-test-stream (stream *LF*)
                  (read-lines stream))))))

(defun run-tests ()
  "Run all :read-line-crlf tests."
  (explain! (run 'test-suite)))
