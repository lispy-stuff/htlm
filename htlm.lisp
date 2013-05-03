(defpackage htlm
  (:export *stack* to-html write-html)
  (:use clite common-lisp core))

(in-package htlm)

(defparameter *stack* nil)

(defparameter <> nil)
(defparameter <<>> nil)
(defparameter <<<>>> nil)

(defclass node ()
  ())

(defgeneric write-html (node out))

(defun to-html (node)
  (with-output-to-string (out)
    (Write-html node out)))

(defclass text-node (node)
  ((content :initarg :content :reader content)))

(defmethod write-html ((node text-node) out)
  (write-string (content node) out))

(defclass elem (node)
   ((child-nodes :initform nil)))

(defgeneric tag (elem))

(defgeneric attrs (elem))

(defun add-child-node (elem child)
  (push child (slot-value elem 'child-nodes)))

(defun child-nodes (elem)
  (reverse (slot-value elem 'child-nodes)))

(defmethod write-html ((node elem) out)
  (format out "<~A" (string-downcase (tag node)))

  (dolist (a (attrs node))
    (let-if (v (rest a))
      (format out " ~A='~A'" (string-downcase (first a)) v)))
  
  (if (child-nodes node)
      (progn
        (write-char #\> out)
        (dolist (c (child-nodes node))
          (write-html c out))
        (format out "</~A>" (string-downcase (tag node))))
      (write-string "/>" out)))

(defmac define-elem (tag supers &rest attrs)
  (let ((class-name (sym tag '-elem))
        (attr-names (mapcar (lambda (a) (if (consp a) (first a) a)) attrs)))
    `(progn
       (defclass ,class-name
           (,@(if supers
                  (mapcar (lambda (s) (sym s '-elem)) supers)
                  '(elem)))
         (,@(mapcar (lambda (a)
                      (if (consp a) 
                          a 
                          `(,a :initform nil :initarg ,(kw a) :accessor ,a)))
                    attrs)))
       
       (defconst ,(sym tag '-attr-names) '(,@attr-names))

       (defmethod tag ((elem ,class-name))
         ,(kw tag))
       
       (defmethod attrs ((elem ,class-name))
         (mapcar (lambda (a) (cons (kw a) (slot-value elem a)))
                 '(,@attr-names))))))

(define-elem html ()
  manifest)

(defmethod write-html :before ((elem html-elem) out)
  (write-string "<!DOCTYPE html>" out))

(define-elem basic ()
  accesskey 
  (class :initform nil :initarg :class :accessor css-class)
  contenteditable
  contextmenu
  dir
  draggable
  dropzone
  hidden
  id
  lang
  onabort
  oncanplay
  oncanplaythrough
  onblur
  onchange
  onclick
  oncontextmenu
  ondblclick
  ondrag
  ondragend
  ondragenter
  ondragleave
  ondragover
  ondragstart
  ondrop
  ondurationchange
  onemptied
  onended
  onerror
  onfocus
  onformchange
  onforminput
  oninput
  oninvalid
  onkeydown
  onkeypress
  onkeyup
  onloadeddata
  onloadedmetadata
  onloadstart
  onmousedown
  onmousemove
  onmouseout
  onmouseover
  onmouseup
  onmousewheel
  onpause
  onplay
  onplaying
  onprogress
  onratechange
  onreadystatechange
  onscroll
  onselect
  onseeked
  onseeking
  onstalled
  onsubmit
  onsuspend
  ontimeupdate
  onvolumechange
  onwaiting
  spellcheck
  style
  tabindex
  title
  translate)

(define-elem a (basic)
  href
  hreflang
  media
  rel
  target
  (type :initform nil :initarg :type :accessor mime-type))

(define-elem body (basic)
  onafterprint
  onbeforeprint
  onbeforeunload
  onhaschange
  onload
  onmessage
  onoffline
  ononline
  onpagehide
  onpageshow
  onpopstate
  onredo
  onresize
  onstorage
  onundo)

(define-elem h (basic))
(define-elem h1 (h))
(define-elem h2 (h))
(define-elem h3 (h))

(defmac do-elem (tag attrs &body body)
  `(progn
     (let ((,$elem (make-instance ',(sym tag '-elem) ,@attrs)))
       (when *stack* (add-child-node (first *stack*) ,$elem))
       (push ,$elem *stack*)
       (let ((<> (first *stack*))
             (<<>> (second *stack*))
             (<<<>>> (third *stack*)))
         ,@body)
       (pop *stack*))))

(defmac do-body-elem (tag attrs &body body)
  `(do-elem ,tag ,attrs
            (macrolet ((a (attrs &body body)
                         `(do-a ,attrs ,@body))
                       (h1 (attrs &body body)
                         `(do-h1 ,attrs ,@body))
                       (text (content)
                         `(add-child-node <>
                                          (make-instance 'text-node
                                                         :content ,content))))
              ,@body)))

(defmac do-a (attrs &body body)
  `(do-body-elem a ,attrs ,@body))

(defmac do-body (attrs &body body)
  `(do-body-elem body ,attrs ,@body))

(defmac do-h1 (attrs &body body)
  `(do-body-elem h1 ,attrs ,@body))

(defmac do-html (&body body)
  `(do-elem html ()
            (macrolet ((body (attrs &body body)
                         `(do-body ,attrs ,@body)))
              ,@body)))

(test (:htlm)
  (test (:empty)
    (do-html
      (body ())
      (test-string= (to-html <>)
                    "<!DOCTYPE html><html><body/></html>")))
  (test (:change :attr)
    (do-a (:href "old link href")
      (test-string= (href <>) "old link href")
      (setf (href <>) "new link href")
      (test-string= (to-html <>)
                    "<a href='new link href'/>")))
  (test (:stack)
    (do-html
      (test-null <<<>>>)
      (test-null <<>>)
      (test-eq (tag <>) :html)
      (body ()
            (test-null <<<>>>)
            (test-eq (tag <<>>) :html)
            (test-eq (tag <>) :body)
            (a ()
               (test-eq (tag <<<>>>) :html)
               (test-eq (tag <<>>) :body)
               (test-eq (tag <>) :a))
            (test-null <<<>>>)
            (test-eq (tag <<>>) :html)
            (test-eq (tag <>) :body))
      (test-null <<<>>>)
      (test-null <<>>)
      (test-eq (tag <>) :html))))