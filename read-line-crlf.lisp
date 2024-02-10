(in-package :read-line-crlf)

(defun read-line-crlf (&optional (stream *standard-input*)
                         (eof-error-p t) eof-value recursive-p 
                         buffer)
  "Similar to CL:READ-LINE except that CR, CRLF, and LF 
are all logical line terminators. See documentation for CL:READ-LINE,
but substitute 'logical line terminator' for 'newline'.

Accepts an optional BUFFER argument in which to accumulate characters
while buffering the line being read, so that you can avoid some consing
when calling READ-LINE in a loop.  If specified, buffer should be as if it were a 
call to (MAKE-STRING n) for N >= 1 (minimum 128 suggested).
The contents of the buffer at any time are unspecified, it may not be used at all.

STREAM must support PEEK-CHAR for CR/CRLF processing."
  (let ((ch (read-char stream eof-error-p eof-value recursive-p)))
    ;; Shortcut for empty lines
    (cond
      ((eql ch #\newline) (return-from read-line-crlf (values "" nil)))
      ((eql ch #\return)  
       (when (eql #\newline (peek-char nil stream nil nil nil))
         (read-char stream nil nil nil)) ;CRLF, discard LF
       (return-from read-line-crlf (values "" nil)))
      ((eql ch eof-value)
       (if eof-error-p
         (error 'end-of-file :stream stream)
         (return-from read-line-crlf (values eof-value t)))))

    ;; Line is not empty
    (let* ((index 1)
           (eof? nil)
           (buffer (or buffer (make-string 128)))
           (buflen (length buffer)))
      (declare (type (simple-array character (*)) buffer)
               (fixnum index buflen))
      (setf (schar buffer 0) (the character ch))
      (loop
        (setf ch (read-char stream eof-error-p eof-value recursive-p))
        (cond 
          ((eql ch eof-value) (setf eof? t) (return))
          ((eql ch #\newline) (return))
          ((eql ch #\return)
           (when (eql #\newline (peek-char nil stream nil nil nil))
             (read-char stream nil nil nil)) ;CRLF, discard LF
           (return))
          (t 
           (when (= index buflen)
             (setf buflen (* buflen 2))
             (let ((new (make-string buflen)))
               (replace new buffer)
               (setf buffer new)))
           (setf (schar buffer index) (the character ch))
           (incf index))))

      ;; Text accumulated.
      (let ((result (make-string index)))
        (replace result buffer :end2 index)
        (values result eof?)))))
