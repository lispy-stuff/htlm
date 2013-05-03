#!/usr/local/bin/sbcl --script
(load #P"core.lisp")
(load #P"clite.lisp")
(load #P"htlm.lisp")

(in-package htlm)

(let ((filename (second sb-ext:*posix-argv*)))
  (with-open-file (out (make-pathname :type "html" :defaults filename)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-html (eval-file filename) out)))