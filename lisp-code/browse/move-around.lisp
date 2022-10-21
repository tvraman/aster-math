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
(proclaim '(inline read-pointer ))
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
(proclaim '(inline previous-read-pointer))
(defun previous-read-pointer() *previous-read-pointer*)
(defmethod read-aloud :before ((document document ))
           "Record this object"
           (setf  *read-pointer*   document)
           )

(proclaim '(inline move-inside-subformula-if-necessary))
(defun move-inside-subformula-if-necessary() 
  "Given that read-pointer is a  subformula with no attributes move
  inside it"
  (when (and (typep *read-pointer* 'math-subformula )
             (null (attributes *read-pointer* )))
    (setf *read-pointer* (contents *read-pointer* )))
  )
(proclaim '(inline move-outside-subformula-if-necessary))
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
      (afl:local-set-state afl:*global-total-audio-state*)
      (if  math-flag
           (with-reading-state (reading-state 'math)
             (afl:local-set-state :math) 
             (read-aloud  *read-pointer* ))
           (read-aloud *read-pointer*))
      (afl:force-speech))
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
      (afl:force-speech )))
    (t  (let ((math-flag (and  (typep *read-pointer* 'math )
                               (not (or (display-math-p *read-pointer*)
                                        (inline-math-p *read-pointer* ))))))
          (afl:new-block
           (afl:local-set-state afl:*global-total-audio-state*)
           (afl:local-set-state)
           (if  math-flag
               (with-reading-state (reading-state 'math)
                                   (afl:local-set-state :math) 
                                   (read-aloud  *read-pointer* ))
             (read-aloud *read-pointer*))
           (setf (afl-state *read-pointer*) nil)
           (afl:force-speech  )))))
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
          (afl:send-text
           (format nil
                   "First ~a. "
                   (type-of *read-pointer*)))))
    (afl:force-speech))
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
          (afl:send-text
           (format nil
                   "Last ~a. "
                   (type-of *read-pointer*)))))
    (afl:force-speech)
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
           (read-aloud "Not that many parent elements. ")))
    (afl:force-speech)
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
                                        ;(t (read-aloud "Nothing to read? "))
    )
  )
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
     ((null move-flag) (read-aloud "No parent defined. ")
      (setf *read-pointer* save-pointer))
     (t (summarize *read-pointer*)
                                        ;         (move-outside-subformula-if-necessary)
        )
     )
    (afl:force-speech ))
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
      (afl:send-text (format nil
                             "Last ~a. "
                             (type-of *read-pointer*)) )
      (setf *read-pointer* save-pointer))
     (t (summarize *read-pointer*)
                                        ;         (move-inside-subformula-if-necessary)
        )
     )
    (afl:force-speech ))
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
       (afl:send-text
        (format nil
                "First ~a. "
                (type-of *read-pointer*)))
       (setf *read-pointer* save-pointer))
      (t (summarize *read-pointer*)
                                        ;         (move-inside-subformula-if-necessary)
         )
      )
    (afl:force-speech ))
  )


(defun read-above ()
  "Read above "
  (read-element-above *read-pointer* )
  (afl:force-speech)
  )

(defmethod  read-element-above ((table-element table-element ))
  "Read element above if present "
(afl:refresh)
  (cond
    ((table-element-above table-element )
     (setf *read-pointer*  (table-element-above table-element  ))
     (read-current-relatively))
    (t (read-aloud "top row. "))
    )
  )


(defun read-below ()
  "Read what is below"
(afl:refresh)
  (read-element-below  *read-pointer* )
  (afl:force-speech )
  )

(defmethod read-element-below ((table-element table-element ))
  "Read element below if present "
(afl:refresh)
  (cond
    ((table-element-below table-element )
     (setf *read-pointer*  (table-element-below table-element  ))
     (read-current-relatively))
    (t (read-aloud "Bottom row. ")))
  )

(defmethod read-element-above ((ordinary t ))
  "Not a table element "
  (read-aloud "not a table element. ")
  )

(defmethod read-element-below ((ordinary t ))
  "Not a table element "
(afl:refresh)
  (read-aloud "not a table element. ")
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
       (read-aloud "top row. "))
  )

(defmethod move-to-element-above ((ordinary t ))
  "No where to go "
  (read-aloud "No above element defined for this object. ")
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
       (read-aloud "bottom row. "))
  )

(defmethod move-to-element-below ((ordinary t))
  "No where to go"
  (read-aloud "Current object does not have an element below defined.
")
  )




(defun read-children ()
  "Read children"
  (afl:refresh)
  (cond
   ((or  (equal 'undefined (children *read-pointer* ))
         (null (children *read-pointer*  )))
    (read-aloud "no children defined. ")
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
   (t (read-aloud "No attributes defined. "))
   )
  )

(defun move-to-children ()
  "Move to children "
  (afl:refresh)
  (cond
   ((or  (equal 'undefined (children *read-pointer* ))
         (null (children *read-pointer*  )))
    (read-aloud "no children defined.  "))
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
   (t (read-aloud "No contents. "))
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
    (afl:force-speech)))
  
(defun move-to-next-in-order()
  "Move to next in reading order"
  (afl:refresh)
  (cond
   ((next-read  *read-pointer* )
    (setf *read-pointer* (next-read *read-pointer* ))
    (summarize *read-pointer*) )
   (t (afl:send-text "Reading order undefined. "))
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
   (t (afl:send-text "Reading order undefined. "))
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
   (t (afl:send-text "Reading order undefined. "))
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
   (t (afl:send-text "Reading order undefined. "))
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
   (t (afl:send-text "no subscript. "))
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
   (t (afl:send-text "no superscript. "))
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
   (t (afl:send-text "no accent. "))
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
   (t (afl:send-text "no underbar. "))
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
   (t (afl:send-text "no left-superscript. "))
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
   (t (afl:send-text "no left-subscript. "))
   )
  )


(defun move-to-abstract ()
  "Move to the abstract. "
  (cond
   ((and (typep *read-pointer* 'article)
         (article-abstract *read-pointer*))
    (setf *read-pointer*
          (article-abstract *read-pointer* )))
   (t (read-aloud "No abstract. "))))

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
      (t (afl:send-text "Not  a cross reference. ")))
     (afl:force-speech))
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
       (afl:send-text "Not in a paragraph."))
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
                               (afl:await-silence)
                               (incf counter))
              (cond
                ((= counter count))
                (t (afl:send-text "Not that many sentences. ")
                   (setf *read-pointer* old-position ))
                )))))
    (afl:force-speech)
    (values))
  )


(defun forward-sentence (&optional (count 1)) 
  "Move forward count sentences. "
  (let
      ((parent (parent *read-pointer* )))
    (cond
      ((not (paragraph-p parent))
       (afl:send-text "Not in a paragraph."))
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
                (t (afl:send-text "Not that many sentences. ")
                   (setf *read-pointer* old-position ))
                )))))
    (afl:force-speech)
    (values))
  )
