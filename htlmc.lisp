#!/usr/local/bin/sbcl --script
(load #P"core.lisp")
(load #P"clite.lisp")
(load #P"htlm.lisp")

(in-package htlm)

(dolist (fn (rest sb-ext:*posix-argv*))
  (let* ((f (open fn))
         (elem (read f)))
    (write-html (eval elem) *standard-output*)))