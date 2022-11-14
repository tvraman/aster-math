;;;   -*- Syntax-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :cl-user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Sun Jan 10 15:47:28 EST 1993
;;; initial attempt at browsing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Record display math objects  as they are read

(defvar *read-pointer* nil "Current object being read. ")

  ;;; Function: READ-POINTER                                   Author: raman
  ;;; Created: Fri Oct 22 09:25:19 1993
(defun read-pointer ()
  "Return current read-pointer position. "
  *read-pointer*
  )


(defmacro save-article (save-variable &optional (article *document*))
  "Save article (default *document*) in save-variable. "
  `(type-of (setf
             ,save-variable ,article )))

(defmacro retrieve-article (save-variable )
  "Retrieve article saved in save-variable and store it in *document*. Does
  not save current value of *document*."
  `(type-of
    (setf  *document*   ,save-variable)))

  ;;; Variable: *PREVIOUS-READ-POINTER*                        Author: raman
  ;;; Created: Sun May 16 14:18:28 1993

(defvar *previous-read-pointer* nil "Previous object, ie object just read. ")
(defun previous-read-pointer() *previous-read-pointer*)

(defun move-inside-subformula-if-necessary()
  "Given that read-pointer is a  subformula with no attributes move
  inside it"
  (when (and (typep *read-pointer* 'math-subformula )
             (null (attributes *read-pointer* )))
    (setf *read-pointer* (contents *read-pointer* )))
  )
(defun move-outside-subformula-if-necessary()
  "Given that read-pointer is a  subformula with no attributes move
  outside  it"
  (when (and (typep *read-pointer* 'math-subformula )
             (null (attributes *read-pointer* )))
    (setf *read-pointer* (parent  *read-pointer* )))
  )

(defun read-current()
  "Read the current selection as specified by the read pointer."
  (afl:refresh)
  (let ((math-flag (and  (typep *read-pointer* 'math )
                         (not (or (display-math-p *read-pointer*)
                                  (inline-math-p *read-pointer* )))))
        )
    (save-pointer-excursion
      (afl:new-block
        (afl:local-set-state afl:*global-speech-state*)
        (if  math-flag
             (with-reading-state (reading-state 'math)
               (afl:local-set-state :math)
               (read-aloud  *read-pointer* ))
             (read-aloud *read-pointer*))
        (afl:tts-force))
      )
    )
  )

(defun read-current-relatively()
  "read current relatively"
  (afl:refresh)
  (save-pointer-excursion
    (cond
      ((afl-state *read-pointer*)
       (afl:new-block
         (afl:local-set-state (afl-state *read-pointer*))
         (read-aloud *read-pointer* )
         (afl:tts-force )))
      (t  (let ((math-flag (and  (typep *read-pointer* 'math )
                                 (not (or (display-math-p *read-pointer*)
                                          (inline-math-p *read-pointer* ))))))
            (afl:new-block
              (afl:local-set-state afl:*global-speech-state*)
              (if  math-flag
                   (with-reading-state (reading-state 'math)
                     (afl:local-set-state :math)
                     (read-aloud  *read-pointer* ))
                   (read-aloud *read-pointer*))
              (setf (afl-state *read-pointer*) nil)
              (afl:tts-force  )))))
    )
  )

;;; Modified: Thu Feb 25 12:06:08 EST 1993
;;; Added optional arg n
;;; <(backed up old version)>

(defun read-previous (&optional(n 1))
  "read previous nth object"
  (afl:refresh)
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (previous *read-pointer* ))
                                               (null (previous *read-pointer*  )))
                                    (setf *read-pointer* (previous *read-pointer*  )))))
    (cond
      (move-flag
       (if (table-element-p *read-pointer* )
           (read-current-relatively)
           (read-current)))
      (t  (setf *read-pointer* save-pointer  )
          (afl:tts-speak
           (format nil
                   "First ~a. "
                   (type-of *read-pointer*)))))
    (afl:tts-force))
  )

;;; Modified: Thu Feb 25 12:08:24 EST 1993
;;; Added optional arg n
;;; <( Backed up old version:2)>

(defun read-next (&optional (n 1 ))
  "Speak the next sibling."
  (afl:refresh)
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (next *read-pointer* ))
                                               (null (next *read-pointer*  )))
                                    (setf *read-pointer* (next *read-pointer*  )))))
    (cond
      (move-flag
       (if (table-element-p *read-pointer*)
           (read-current-relatively)
           (read-current )))
      (t  (setf *read-pointer* save-pointer  )
          (afl:tts-queue
           (format nil
                   "Last ~a. "
                   (type-of *read-pointer*)))))
    (afl:tts-force)
    )
  )

;;; Modified: Thu Feb 25 12:10:44 EST 1993
;;; added optional arg n

(defun read-parent (&optional(n 1))
  "Speak the parent."
  (afl:refresh)
  (let ((save-pointer *read-pointer* )
        (move-flag nil ))
    (setf move-flag
          (loop for i from 1 to n always
                                  (unless (or  (equal 'undefined (parent *read-pointer* ))
                                               (null (parent *read-pointer*  )))
                                    (setf *read-pointer* (parent *read-pointer*  )))))
    (if move-flag
        (read-current)
        (and (setf *read-pointer* save-pointer  )
             (afl:tts-speak "Not that many parent elements. ")))
    (afl:tts-force)
    )
  )

  ;;; Function: READ-REST                                      Author: raman
  ;;; Created: Wed Sep  8 11:06:09 1993

(defun read-rest (start-position &optional (read-this-node t))
  "Read rest of document. "
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
      (t (summarize *read-pointer*)
                                        ;         (move-inside-subformula-if-necessary)
         )
      )
    (afl:tts-force ))
  )

(defun move-back(&optional(n 1))
  "Move to previous sibling."
  (afl:refresh)
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
  (afl:refresh)
  (cond
    ((table-element-above table-element )
     (setf *read-pointer*  (table-element-above table-element  ))
     (read-current-relatively))
    (t (afl:tts-speak "top row. "))
    )
  )

(defun read-below ()
  "Read what is below"
  (afl:refresh)
  (read-element-below  *read-pointer* )
  (afl:tts-force )
  )

(defmethod read-element-below ((table-element table-element ))
  "Read element below if present "
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  "Move read pointer to contents"
  (afl:refresh)
  (cond
    ((listp (contents  *read-pointer*))
     (setf *read-pointer* (first (contents  *read-pointer* )))
     (summarize *read-pointer* ))
    ((and (contents *read-pointer*)
          (not (eql 'undefined (contents *read-pointer* ))))
     (setf *read-pointer* (contents
                           *read-pointer* ))
     (summarize *read-pointer*))
    (t (afl:tts-speak "No contents. "))
    )
  )

  ;;; Method: READ-NODE                                        Author: raman
  ;;; Created: Mon Mar 29 08:35:57 1993

(defmethod read-node ((math-object math-object))
  "Read this node"
  (save-pointer-excursion
    (read-math-object-and-attributes *read-pointer*))
  )

(defmethod read-node((object t))
  "Default method for read node"
  (when (contents *read-pointer*)
    (save-pointer-excursion (read-aloud (contents *read-pointer* ))))
  )

(defun read-just-the-node()
  "Just read this node"
  (afl:refresh)
  (let ((save-children (children *read-pointer*)))
    (unwind-protect
         (save-pointer-excursion
           (setf (children *read-pointer*) nil)
           (read-aloud *read-pointer*))
      (setf (children *read-pointer*) save-children ))
    (afl:tts-force)))

(defun move-to-next-in-order()
  "Move to next in reading order"
  (afl:refresh)
  (cond
    ((next-read  *read-pointer* )
     (setf *read-pointer* (next-read *read-pointer* ))
     (summarize *read-pointer*) )
    (t (afl:tts-queue "Reading order undefined. "))
    )
  (values)
  )

(defun move-to-previous-in-order()
  "Move to previous in reading order"
  (afl:refresh)
  (cond
    ((previous-read  *read-pointer* )
     (setf *read-pointer* (previous-read *read-pointer* ))
     (summarize *read-pointer*) )
    (t (afl:tts-queue "Reading order undefined. "))
    )
  (values)
  )

(defun read-next-in-order()
  "Read next in reading order"
  (afl:refresh)
  (cond
    ((next-read  *read-pointer* )
     (setf *read-pointer* (next-read *read-pointer* ))
     (read-current) )
    (t (afl:tts-queue "Reading order undefined. "))
    )
  (values)
  )

(defun read-previous-in-order()
  "Read previous  in reading order"
  (afl:refresh)
  (cond
    ((previous-read  *read-pointer* )
     (setf *read-pointer* (previous-read *read-pointer* ))
     (read-current) )
    (t (afl:tts-queue "Reading order undefined. "))
    )
  (values)
  )

(defun move-to-top-of-math()
  "Move to top of math"
  (afl:refresh)
  (loop while (typep (parent  *read-pointer*) 'math) do
    (setf *read-pointer* (parent *read-pointer* )))
  (summarize *read-pointer*)
  )

(defun move-to-subscript ()
  "move to subscript. "
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
  (afl:refresh)
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
          (article-abstract *read-pointer*))
     (setf *read-pointer*
           (article-abstract *read-pointer* )))
    (t (afl:tts-speak "No abstract. "))))

(defun read-follow-cross-ref(direction-flag)
  "Follow and read the closest cross reference. "
  (afl:refresh)
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

  ;;; Function: FORWARD-SENTENCE                               Author: raman
  ;;; Created: Thu May 20 17:05:08 1993

(defun read-sentence (&optional (count 1))
  "Read  count sentences."
  (let
      ((parent (parent *read-pointer* )))
    (cond
      ((not (paragraph-p parent))
       (afl:tts-queue "Not in a paragraph."))
      (t  (let ((counter 0)
                (old-position *read-pointer*))
            (flet ((read-to-end-of-sentence()
                     (let ((next (next *read-pointer* )))
                       (loop while
                             (and   (not (end-of-sentence? *read-pointer* ))
                                    next)
                             do (save-pointer-excursion
                                  (read-aloud next))
                                (setf *read-pointer* next)
                                (setf  next (next next )))
                       (when next (setf *read-pointer* next )))))
              (loop while (and (next *read-pointer* )
                               (< counter count)) do
                                 (save-pointer-excursion
                                   ( read-aloud *read-pointer*))
                                 (read-to-end-of-sentence)
                                 (afl:tts-force)
                                 (incf counter))
              (cond
                ((= counter count))
                (t (afl:tts-queue "Not that many sentences. ")
                   (setf *read-pointer* old-position ))
                )))))
    (afl:tts-force)
    (values))
  )

(defun forward-sentence (&optional (count 1))
  "Move forward count sentences. "
  (let
      ((parent (parent *read-pointer* )))
    (cond
      ((not (paragraph-p parent))
       (afl:tts-queue "Not in a paragraph."))
      (t  (let  ((counter 0)
                 (old-position *read-pointer*)
                 (mover (if (minusp count) #'previous #'next))
                 (limit (abs count )))
            (flet ((move-to-end-of-sentence(mover)
                     (let ((next (funcall mover  *read-pointer* )))
                       (loop while
                             (and   (not (end-of-sentence? *read-pointer* ))
                                    next)
                             do (setf *read-pointer* next)
                                (setf  next (funcall mover  next )))
                       (when next (setf *read-pointer* next ))))
                   (beginning-of-sentence()
                     (let ((previous  (previous   *read-pointer* )))
                       (loop while
                             (and   (not (end-of-sentence? *read-pointer* ))
                                    previous)
                             do (setf *read-pointer* previous)
                                (setf  previous  (previous   previous )))
                       (when previous  (setf *read-pointer*
                                             (next *read-pointer* )))))
                   );end flet functions
              (loop while (and (funcall mover  *read-pointer* )
                               (< counter limit)) do
                                 (move-to-end-of-sentence mover)
                                 (incf counter))
              (cond
                ((= counter limit)
                 (when (minusp count)
                   (beginning-of-sentence )))
                (t (afl:tts-queue "Not that many sentences. ")
                   (setf *read-pointer* old-position ))
                )))))
    (afl:tts-force)
    (values))
  )
