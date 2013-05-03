;Copyright (c) 2012, trialcodr <adde@trialcode.com>

;Permission to use, copy, modify, and/or distribute this software for any
;purpose with or without fee is hereby granted, provided that the above
;copyright notice and this permission notice appear in all copies.

;THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
;REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
;AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
;INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
;OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;PERFORMANCE OF THIS SOFTWARE.

(defpackage clite
  (:export run-tests
           test
           test= test-eq test-eql test-equal test-equalp
           test-fail test-null test-pass
           test-string= testp)
  (:use common-lisp core))

(in-package clite)

(defparameter running-tags nil)
(defparameter test-count 0)
(defparameter tests nil)

(defun run? (test-tags running-tags &key null-test-result)
  "Return T if TEST-TAGS matches RUNNING-TAGS, second result value contains the
remaining RUNNING-TAGS"
  (cond
    ((null running-tags)
     (values t running-tags))
    ((null test-tags)
     (values null-test-result running-tags))
    ((eq (car test-tags) (car running-tags))
     (run? (cdr test-tags) (cdr running-tags) :null-test-result null-test-result))
    (t (run? (cdr test-tags) running-tags))))

(defmac test (tags &body body)
  "Define test for TAGS"
  `(macrolet ((test (tags &body body)
                `(multiple-value-bind (run? running-tags)
                     (run? ',tags running-tags)
                   (when run?
                     ,@body))))
     (let ((,$found (assoc ',tags tests :test #'equal))
           (,$test-form (lambda ()
                          ,@body)))
       (if ,$found
             (setf (cdr ,$found) ,$test-form)
             (push (cons ',tags ,$test-form) tests)))))

(defun test-fail (&optional msg &rest args)
  "Signal test failure"
  (if msg
      (error "Test failed: ~a" (apply #'format nil msg args))
      (error "Test failed")))

(defun test-pass ()
  "Signal passed test"
  (incf test-count)
  (princ #\.))

(defun testp (condition &optional msg)
  "Signal test failure if CONDITION is false"
  (unless condition (test-fail msg))
  (test-pass))

(defun test-null (condition &optional msg)
  "Signal test failure if CONDITION is true"
  (unless (null condition) (test-fail msg))
  (test-pass))

(defmac define-ops (&rest ops)
  "Generate test versions of OPS"
  (flet ((test-form-name (op)
           (if (= (length (str op)) 1)
               (sym 'test op)
               (sym 'test- op))))
    `(progn
       ,@(mapcar (lambda (op)
                   `(defun ,(test-form-name op)
                        (left right &optional message)
                      (unless (funcall #',op left right)
                        (test-fail "~A (~A ~S ~S)" message ',op left right))
                      (test-pass)))
                 ops))))

(define-ops = eq eql equal equalp string=)

(defmacro run-tests (&rest tags)
  "Run TESTS matching TAGS"
  `(progn
     (format t "~&Testing ~S" ',tags)
     (let ((test-count 0)
           (running-tags ',tags))
       (dolist-reverse (test tests)
         (let ((test-tags (car test)))
           (multiple-value-bind (run? running-tags)
               (run? test-tags running-tags)
             (when run? (funcall (cdr test))))))
       (format t "Done!~%")
       test-count)))