# read-line-crlf

A READ-LINE alternative that handles MS/Mac/Unix end-of-line conventions.

The Common Lisp `read-line` function only looks for linefeed characters as
the basis of line termination. In Common Lisp's defense, linefeeds were an
entirely adequate convention for text line termination, Microsoft hadn't
been around long enough to screw everything up, and Apple wasn't even a gleam in
anybody's eye. Ah well, progress, or failing that, some printer did the
right thing with the right EOL conventions in some context at some time.

This module provides a function, `read-line-crlf`, which treats any of the
following sequences as a _single_ line separation.

    LF, CR, CRLF

In particular, CRLF indicates ONE logical end-of-line.

Have a nice day.

# Usage 

If you didn't get this via quicklisp from the quicklisp repo (because it isn't
there yet), add it to your `~/quicklisp/localprojects/` directory, update/wipe
the `system-index.txt` file accordingly, and then you can quickload it.

    ;; See 'local-projects' note in preceding paragraph
    (ql:quickload :read-line-crlf) ; to use the code

And from there you can import/use the exported `read-line-crlf`
function anywhere you would otherwise use `read-line`.

To run tests:

    ;; (again, see 'local-projects' note if applicable)
    (ql:quickload :read-line-crlf-test)
    (read-line-crlf-test:run-tests)

# The BUFFER parameter

`read-line-crlf` takes an optional `buffer` parameter after all the standard
READ-LINE parameters.  If you're calling it
in a loop you can avoid some consing by passing a larger buffer, e.g.

    (let ((buffer (make-string 1024)))
      (loop ...
        (read-line-crlf stream nil nil nil buffer) ...))

# Tested Lisps

SBCL, CCL, ABCL, Allegro CL, ECL were tested and working fine with
relatively recent releases as of Feb 10 2024.

LispWorks 8 works as well, but I'm unsure how to declare the buffer for 
optimal effect (not that I've done that correctly for other lisps, but at
least they don't break), so the buffer type declaration is omitted for LispWorks.
See `read-line-crlf` function code for more details.
