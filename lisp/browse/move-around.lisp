;;;   -*- Syntax-Lisp;  Base: 10; Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

(export
 '(move-back move-forward move-up move-to-children
   move-to-abstract move-to-attributes
   move-to-contents
   move-to-subscript move-to-superscript
   move-to-math-root move-to-doc-root
   read-current read-next read-previous read-parent))

;;; Sun Jan 10 15:47:28 EST 1993

;;; Record objects  as they are read

(defvar *read-pointer* nil "Current object being read. ")

(defun read-current()
  "Read the current selection."
  (let ((math-flag (and  (typep *read-pointer* 'math )
                         (not (or (display-math-p *read-pointer*)
                                  (inline-math-p *read-pointer* ))))))
    (save-pointer-excursion
      (afl:new-block
        (if  math-flag
             (with-reading-state (reading-state 'math)
               (afl:set-pronunciation-mode :math)
               (read-aloud  *read-pointer* ))
             (read-aloud *read-pointer*))
        (afl:tts-force)))))

(defun read-current-relatively()
  "read current relatively"
  (save-pointer-excursion
    (cond
      ((afl-state *read-pointer*)
       (afl:new-block
         (afl:local-set-state (afl-state *read-pointer*))
         (read-aloud *read-pointer* )
         (afl:tts-force )))
      (t
       (let ((math-flag
               (and  (typep *read-pointer* 'math )
                     (not (or (display-math-p *read-pointer*)
                              (inline-math-p *read-pointer* ))))))
         (afl:new-block
           (if  math-flag
                (with-reading-state (reading-state 'math)
                  (afl:set-pronunciation-mode :math)
                  (read-aloud  *read-pointer* ))
                (read-aloud *read-pointer*))
           (setf (afl-state *read-pointer*) nil)
           (afl:tts-force  )))))))

(defun read-previous (&optional(n 1))
  "read previous sibling."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop
            for i from 1 to n
            always
            (unless (or  (equal 'undefined (previous *read-pointer* ))
                         (null (previous *read-pointer*  )))
              (setf *read-pointer* (previous *read-pointer*  )))))
    (cond
      (move-flag
       (if (table-element-p *read-pointer* )
           (read-current-relatively)
           (read-current)))
      (t
       (afl:tts-speak
        (format nil
                "First ~a. "
                (type-of *read-pointer*)))))
    (afl:tts-force)))

(defun read-next (&optional (n 1 ))
  "Read next sibling."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop
            for i from 1 to n
            always
            (unless (or  (equal 'undefined (next *read-pointer* ))
                         (null (next *read-pointer*  )))
              (setf *read-pointer* (next *read-pointer*  )))))
    (cond
      (move-flag
       (if (table-element-p *read-pointer*)
           (read-current-relatively)
           (read-current )))
      (t
       (afl:tts-queue
        (format nil
                "Last ~a. "
                (type-of *read-pointer*)))))
    (afl:tts-force)))

(defun read-parent (&optional(n 1))
  "Speak the parent."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop
            for i from 1 to n
            always
            (unless (or  (equal 'undefined (parent *read-pointer* ))
                         (null (parent *read-pointer*  )))
              (setf *read-pointer* (parent *read-pointer*  )))))
    (if move-flag
        (read-current)
        (and (setf *read-pointer* save-pointer  )
             (afl:tts-speak "Not that many parent elements. ")))
    (afl:tts-force)))

  ;;; Function: READ-REST                                      Author: raman
  ;;; Created: Wed Sep  8 11:06:09 1993

(defun read-rest (start-position &optional (read-this-node t))
  "Read rest of document. "
  (when read-this-node
    (read-aloud start-position))
  (cond
    ((and (next start-position)
          (not (equal 'undefined (next start-position ))))
     (read-rest (next start-position )))
    ((and (parent  start-position)
          (not (equal 'undefined (parent start-position ))))
     (read-rest  (parent  start-position) nil))
    (t nil)))

(defun move-up (&optional(n 1))
  "Move to parent."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (parent *read-pointer* ))
                                               (null (parent *read-pointer*  )))
                                    (setf *read-pointer* (parent *read-pointer*  )))))
    (cond
      ((null move-flag) (afl:tts-speak "No parent defined. ")
       (setf *read-pointer* save-pointer))
      (t (summarize *read-pointer*)
                                        ;         (move-outside-subformula-if-necessary)
         )
      )
    (afl:tts-force ))
  )

(defun move-forward (&optional(n 1))
  "Move to next sibling."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (next *read-pointer* ))
                                               (null (next *read-pointer*  )))
                                    (setf *read-pointer* (next *read-pointer*  )))))
    (cond
      ((null move-flag)
       (afl:tts-queue (format nil
                              "Last ~a. "
                              (type-of *read-pointer*)) )
       (setf *read-pointer* save-pointer))
      (t (summarize *read-pointer*)))
    (afl:tts-force )))

(defun move-back(&optional(n 1))
  "Move to previous sibling."
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (previous *read-pointer* ))
                                               (null (previous *read-pointer*  )))
                                    (setf *read-pointer* (previous *read-pointer*  )))))
    (cond
      ((null move-flag)
       (afl:tts-queue
        (format nil
                "First ~a. "
                (type-of *read-pointer*)))
       (setf *read-pointer* save-pointer))
      (t (summarize *read-pointer*)
                                        ;         (move-inside-subformula-if-necessary)
         )
      )
    (afl:tts-force ))
  )

(defun read-above ()
  "Read above "
  (read-element-above *read-pointer* )
  (afl:tts-force)
  )

(defmethod  read-element-above ((table-element table-element ))
  "Read element above if present "
  (cond
    ((table-element-above table-element )
     (setf *read-pointer*  (table-element-above table-element  ))
     (read-current-relatively))
    (t (afl:tts-speak "top row. "))
    )
  )

(defun read-below ()
  "Read what is below"
  (read-element-below  *read-pointer* )
  (afl:tts-force )
  )

(defmethod read-element-below ((table-element table-element ))
  "Read element below if present "
  (cond
    ((table-element-below table-element )
     (setf *read-pointer*  (table-element-below table-element  ))
     (read-current-relatively))
    (t (afl:tts-speak "Bottom row. ")))
  )

(defmethod read-element-above ((ordinary t ))
  "Not a table element "
  (afl:tts-speak "not a table element. ")
  )

(defmethod read-element-below ((ordinary t ))
  "Not a table element "
  (afl:tts-speak "not a table element. ")
  )

(defun move-above()
  "Move to above object"
  (move-to-element-above *read-pointer* )
  )

(defmethod move-to-element-above((table-element table-element ))
  "Move to above element"
  (if  (table-element-above table-element )
       (type-of (setf *read-pointer*  (table-element-above
                                       table-element  )))
       (afl:tts-speak "top row. "))
  )

(defmethod move-to-element-above ((ordinary t ))
  "No where to go "
  (afl:tts-speak "No above element defined for this object. ")
  )

(defun move-below ()
  "Move  to element below"
  (move-to-element-below *read-pointer* )
  )

(defmethod move-to-element-below ((table-element table-element ))
  "Move to element below"
  (if  (table-element-below table-element )
       (type-of (setf *read-pointer* (table-element-below
                                      table-element )))
       (afl:tts-speak "bottom row. "))
  )

(defmethod move-to-element-below ((ordinary t))
  "No where to go"
  (afl:tts-speak "Current object does not have an element below defined.
")
  )

(defun read-children ()
  "Read children"
  (cond
    ((or  (equal 'undefined (children *read-pointer* ))
          (null (children *read-pointer*  )))
     (afl:tts-speak "no children defined. ")
     )
    (t (setf  *read-pointer* (children *read-pointer* ))
       (read-current))
    )
  )

(defun move-to-attributes()
  "Move to attributes"
  (cond
    ((and (typep *read-pointer* 'math-object)
          (attributes *read-pointer*))
     (setf *read-pointer* (attributes *read-pointer*))
     (summarize *read-pointer*))
    (t (afl:tts-speak "No attributes defined. "))
    )
  )

(defun move-to-children ()
  "Move to children "
  (cond
    ((or  (equal 'undefined (children *read-pointer* ))
          (null (children *read-pointer*  )))
     (afl:tts-speak "no children defined.  "))
    ((listp (children *read-pointer*))
     (setf *read-pointer* (first (children *read-pointer* )))
     (summarize *read-pointer* )
                                        ;     (move-inside-subformula-if-necessary)
     )
    (t  (setf *read-pointer* (children *read-pointer*)
              )
        (summarize *read-pointer* )
                                        ;        (move-inside-subformula-if-necessary)
        )
    ))

(defun move-to-contents()
  "Move to contents"
  (cond
    ((listp (contents  *read-pointer*))
     (setf *read-pointer* (first (contents  *read-pointer* )))
     (summarize *read-pointer* ))
    ((and (contents *read-pointer*)
          (not (eql 'undefined (contents *read-pointer* ))))
     (setf *read-pointer* (contents
                           *read-pointer* ))
     (summarize *read-pointer*))
    (t (afl:tts-speak "No contents. "))))

(defun move-to-math-root ()
  "Move to top of math"
  (loop while (typep (parent  *read-pointer*) 'math) do
    (setf *read-pointer* (parent *read-pointer* )))
  (summarize *read-pointer*)
  )

(defun move-to-doc-root ()
  "Move to document root."
  (setf *read-pointer* *document*)
  (summarize *read-pointer*)
  (afl:tts-force))

(defun move-to-subscript ()
  "move to subscript. "
  (cond
    ((subscript *read-pointer* )
     (setf *read-pointer* (parent ( subscript *read-pointer* )))
                                        ; superscript return the value so need to move up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no subscript. "))
    )
  )

(defun move-to-superscript ()
  "move to superscript. "
  (cond
    ((superscript *read-pointer* )
     (setf *read-pointer* (parent  (superscript *read-pointer* )))
                                        ; superscript returns the value so need to go up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no superscript. "))
    )
  )

(defun move-to-accent ()
  "move to accent. "
  (cond
    ((accent *read-pointer* )
     (setf *read-pointer* (parent  (accent *read-pointer* )))
                                        ; accent returns the value so need to go up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no accent. "))
    )
  )

(defun move-to-underbar ()
  "move to underbar. "
  (cond
    ((underbar *read-pointer* )
     (setf *read-pointer* (parent  (underbar *read-pointer* )))
                                        ; underbar returns the value so need to go up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no underbar. "))
    )
  )

(defun move-to-left-superscript ()
  "move to left-superscript. "
  (cond
    ((left-superscript *read-pointer* )
     (setf *read-pointer* (parent  (left-superscript *read-pointer* )))
                                        ; left-superscript returns the value so need to go up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no left-superscript. "))
    )
  )
(defun move-to-left-subscript ()
  "move to left-subscript. "
  (cond
    ((left-subscript *read-pointer* )
     (setf *read-pointer* (parent  (left-subscript *read-pointer* )))
                                        ; left-subscript returns the value so need to go up
     (summarize *read-pointer* ))
    (t (afl:tts-queue "no left-subscript. "))
    )
  )

(defun move-to-abstract ()
  "Move to the abstract. "
  (cond
    ((and (typep *read-pointer* 'article)
          (abstract *read-pointer*))
     (setf *read-pointer*
           (abstract *read-pointer* ))
     (read-aloud *read-pointer*))
    (t (afl:tts-speak "No abstract. "))))

(defun read-follow-cross-ref(direction-flag)
  "Follow and read the closest cross reference. "
  (save-pointer-excursion
    (let
        ((successor (if direction-flag  #'previous #'next  )))
      (loop while (and  (funcall successor *read-pointer*)
                        (not (typep *read-pointer* 'cross-ref )))
            do (setf *read-pointer* (funcall successor *read-pointer* )))
      (cond
        ((typep *read-pointer* 'cross-ref)
         (follow-cross-reference *read-pointer*))
        (t (afl:tts-queue "Not  a cross reference. ")))
      (afl:tts-force))
    )
  )
