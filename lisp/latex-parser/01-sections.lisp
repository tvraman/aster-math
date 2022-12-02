;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(in-package :aster)

(defvar *document* nil
  "Cache document pointer used in browser.")


;;; Variable: *valid-section-names*                   Author: raman
;;; Created: Thu Apr  9 15:50:50 1992

(defvar *valid-sectional-unit-names*
  '(part chapter section subsection subsubsection)
  "list of valid sectioning units")


;;; Modified: Thu Apr  9 19:49:28 EDT 1992
;;; Introduced two new functions:
;;; create-sectional-unit and get-sectional-units!
;;; which make all the different create-[sub]section functions obselete.
;;; Modified: Thu Apr  2 15:40:22 EST 1992
;;; Modifying to use classes for document components instead of
;;; structures.

;;; New version:  Sun Jan 26 09:37:20 EST 1992
;;; Made to work with a more intelligent lexer.
;;; The tokenizer does more work,
;;; reducing the acrobatics that were being performed by lisp in the older
;;; version.

;;; This version is also being made table driven.

;;; The functions in this file are being reimplemented using the buffer
;;; structure defined in buffers.lisp.

;;; Conventions:
;;; Function names ending in ! such as get-title! change the buffer pointer
;;; of the buffer passed to them as an argument.
;;; Same is true of function names which begin with the word pop.
;;; eg: pop-when-true
;;; Predicates have names ending  in `?'.
;;; Functions which expect a buffer as an argument have their arg names
;;; ending in buffer, eg: text-buffer.
;;; Function: CREATE-ARTICLE                                Author: raman
;;; Created: Fri Oct 11 10:02:00 1991

(defun create-article (text-article)
  "Creates an  article from lispified text."
  (initialize-cross-references)
  (initialize-counters)
  (let ((text-buffer (make-buffer :contents text-article )))
    (when
        (eq 'document (lookat-current-entry text-buffer)) (advance-pointer text-buffer)
        (let ((new-article (make-article )))
          (setf (contents new-article) (process-initial-body! text-buffer))
          (setf (children new-article)
                (number-list-sectional-units
                 (get-sectional-units! text-buffer
                                       :sectional-unit-name
                                       (name-of-sectional-unit-in-front text-buffer))))
          (setf (references new-article) (get-references! text-buffer))
          (link-children-to-parent new-article)))
    new-article))

;;; Variable: *COUNTER-TABLE*                                Author: raman
;;; Created: Fri Apr 30 10:40:37 1993

(defvar *counter-table* (make-hash-table :test #'equal) "Table of counters")

(defun counter-value (name)
  "Return counter value."
  (declare (special *counter-table*))
  (or (gethash name *counter-table*) (setf (gethash name
                                                    *counter-table*)
                                           1)))

;;; Function: NAME-OF-SECTIONAL-UNIT-IN-FRONT                Author: raman
;;; Created: Fri Oct  9 13:08:11 1992

(defun name-of-sectional-unit-in-front (text-buffer)
  "Return name of sectional unit in front of buffer"
  (and
   (listp (lookat-current-entry text-buffer))
   (find
    (what-is?  (lookat-current-entry text-buffer))
    *valid-sectional-unit-names*)))
;;; Function: process-INITIAL-BODY!                             Author: raman
;;; Created: Tue Jan 28 12:16:33 1992

(defun process-initial-body! (text-buffer)
  "Return initial  body of the article,  ie upto first section."
  (delete nil
   (process-text text-buffer
    #'(lambda(x)
        (or (name-of-sectional-unit-in-front x)
         (end-of-buffer? x))))))

;;; Function: GET-REFERENCES!                                 Author: raman
;;; Created: Fri Oct 11 11:09:02 1991

(defun get-references! (text-buffer)
  "Get references in document. "
  (pop-while-true text-buffer
                  #'(lambda(x) (is-a  'reference x))))
;;; Function: CHILD-OF-SECTIONAL-UNIT                        Author: raman
;;; Created: Thu Apr  9 17:32:48 1992

(defun child-of-sectional-unit (sectional-unit-name)
  "return name of the child of this unit"
  (when (exists-child-of-sectional-unit? sectional-unit-name)
    (elt *valid-sectional-unit-names*
         (1+ (position sectional-unit-name *valid-sectional-unit-names*)))))

;;; Function: EXISTS-CHILD-OF-SECTIONAL-UNIT?                Author: raman
;;; Created: Thu Apr  9 20:06:38 1992

(defun exists-child-of-sectional-unit? (sectional-unit-name)
  "Sees if child posible "
  (< (1+ (position sectional-unit-name *valid-sectional-unit-names*))
     (length *valid-sectional-unit-names*)))

;;; Function: VALIDATE-SECTIONAL-UNIT-NAME Author: raman
;;; Created: Thu Apr  9 15:48:21 1992

(defun validate-sectional-unit-name (unit-name)
  "check if unit-name is a valid sectional unit"
  (or
   (find unit-name *valid-sectional-unit-names* )
   (error "~a is not a valid  sectional unit name" unit-name)))

  ;;; Variable: *CROSS-REFERENCES*                             Author: raman
  ;;; Created: Mon Dec 28 14:05:31 1992

(defvar *cross-references* nil
  "Holds cross reference labels ")

  ;;; Function: INITIALIZE-CROSS-REFERENCES                    Author: raman
  ;;; Created: Mon Dec 28 19:18:31 1992

(defun initialize-cross-references ()
  "Initialize cross reference table"
  (setf *cross-references* (make-hash-table :test #'equal )))

(defun cross-reference-table () *cross-references*)

  ;;; Method: INSTALL-LABEL                                    Author: raman
  ;;; Created: Mon Dec 28 15:02:13 1992

(defmethod install-label ((label label))
  "Install label in the global cross reference table"
  (let ((label-tag (contents label )))
    (flet (
           (make-label-name(object)
             (let ((label-name
                     (if(and (typep  object 'new-environment)
                             (eql (class-name (class-of object))
                                  'new-environment))
                        (name object)
                        (class-name (class-of object )))))
               (format nil "~a ~a"
                       label-name
                       (counter-value label-name )))))
      (setf (parent label)  (current-referend))
      (unless (eq 'undefined (label  (current-referend )))
        (setf (label (current-referend))  label ))
      (setf (label-name label)  (make-label-name (current-referend )))
      (setf (gethash label-tag *cross-references* )  label )
      (values)
      )
    ))


;;; Function: INITIALIZE-COUNTERS                            Author: raman
;;; Created: Fri Apr 30 11:15:35 1993

(defun initialize-counters ()
  "Initialize table of counters"
  (setf *counter-table* (make-hash-table :test #'equal)))

;;; Function: INCREMENT-COUNTER-VALUE                        Author: raman
;;; Created: Sun May  2 09:31:29 1993

(defun increment-counter-value (counter-name)
  "Increment value of this counter. "
  (let ((value (gethash counter-name *counter-table* )))
    (cond
      ((null value)
       (setf (gethash counter-name *counter-table*) 1)
       1)
      (t (incf (gethash counter-name *counter-table*))))))

  ;;; Function: LABELLED-P                                     Author: raman
  ;;; Created: Fri Apr 30 09:16:04 1993
;;; Modified: Fri Apr 30 11:18:13 EDT 1993
;;; Since objects that can be labelled have a label slot now, use
;;; this.

(defun labelled-p (object)
  "Has this object been labelled?"
  (cond
    ((eq 'undefined (label object )) nil)
    ( (label object ) )
    (t (error "Should not have got here: "))
    )
  )

;;; Latex treats labels by having certain environments that are
;;; capable of being labelled and places the label on the
;;; enclosing environment. This means that the label can appear
;;; anywhere in the input, not just in the beginning of such
;;; environments.

;;; Have a variable *enclosing-referends* which keeps a trail of the
;;; enclosing referends.  Have a variable
;;; *objects-that-can-be-referred*  that records the names of those
;;; objects that can be referred to. Then when processing such an
;;; object, first add that object to the list of *enclosing-referends*
;;; and make label-expand install the label by looking at the front of
;;; this list. This means that label-expand will return nil.

  ;;; Variable: *OBJECTS-THAT-CAN-BE-REFERRED*                 Author: raman
  ;;; Created: Tue Dec 29 09:16:04 1992
;;; external variable:
(defvar *objects-that-can-be-referred*
  '(new-environment  math-eqnarray
    math-equation part chapter section subsection subsubsection enumerate item )
  "These objects can be referred to by cross references")

  ;;; Function: CAN-THIS-BE-CROSS-REFERENCED?                       Author: raman
  ;;; Created: Tue Dec 29 09:24:50 1992

(defun can-this-be-cross-referenced? (object-name)
  "Can this be cross referenced?"
  (find object-name *objects-that-can-be-referred* )
  )

  ;;; Variable: *ENCLOSING-REFERENDS*                          Author: raman
  ;;; Created: Tue Dec 29 09:17:41 1992

(defvar *enclosing-referends* nil
  "List of current enclosing objects that can be referred to")

  ;;; Function: ADD-ENCLOSING-REFEREND                         Author: raman
  ;;; Created: Tue Dec 29 09:18:27 1992

(defun add-enclosing-referend (object)
  "Add this to enclosing referends"
  (push object *enclosing-referends*)
  )

  ;;; Function: POP-ENCLOSING-REFEREND                         Author: raman
  ;;; Created: Tue Dec 29 09:19:24 1992

(defun pop-enclosing-referend ()
  "Pop enclosing referend"
  (pop *enclosing-referends*)
  )

  ;;; Function: CURRENT-REFEREND                               Author: raman
  ;;; Created: Tue Dec 29 09:20:05 1992

(defun current-referend ()
  "Return current referend"
  (first *enclosing-referends*)
  )

  ;;; Method: CROSS-REFERENCE-KEY                              Author: raman
  ;;; Created: Tue Dec 29 11:16:42 1992

(defun  cross-reference-key (cross-ref)
  "Pick out the cross reference key as a string"
  (let
      ((key (first (children  cross-ref ))))
    (cond
      ((stringp  key ) key)
      ((text-block-p key) (contents key  ))
      ((and (listp key)
            (text-block-p (first key  )))
       (contents (first key  ))))))

  ;;; Method: FIND-CROSS-REFERENCE                             Author: raman
  ;;; Created: Mon Dec 28 15:12:12 1992

(defmethod find-cross-reference ((cross-ref cross-ref))
  "Find the cross reference refered to by ref"
  (let ((reference-key (cross-reference-key cross-ref )))
    (gethash  reference-key  *cross-references* ))
  )

  ;;; Function: MAKE-PARAGRAPH-IF-NECESSARY                    Author: raman
  ;;; Created: Fri Oct 22 13:07:39 1993

(defun make-paragraph-if-necessary (input-list)
  "Make this input into a paragraph if necessary."
  (cond
    ((and (listp input-list)
          (not (paragraph-p (first input-list ))))
     (list (make-instance 'paragraph
                          :contents input-list)))
    (t input-list))
  )

;;; Function: CREATE-SECTIONAL-UNIT                          Author: raman
;;; Created: Thu Apr  9 16:01:07 1992
;;; Modified: Tue Dec 29 09:23:00 EST 1992
;;; Using enclosing-referends to handling labels.
;;; <(old version has been backed up. )>

(defun create-sectional-unit (sectional-unit-text
                              &key(sectional-unit-name 'section))
  "Create a sectional unit, default is a section."
  (let ((sectional-unit-buffer (make-buffer :contents
                                            sectional-unit-text )))
    (when
        (eq sectional-unit-name (lookat-current-entry sectional-unit-buffer))
      (advance-pointer sectional-unit-buffer)
      (let ((new-sectional-unit  (make-sectional-unit
                                  :sectional-unit-name
                                  sectional-unit-name))
            )
        (when (can-this-be-cross-referenced? sectional-unit-name)
          (add-enclosing-referend new-sectional-unit))
        (setf (title new-sectional-unit) (get-unit-title!  sectional-unit-buffer))
        (setf (body new-sectional-unit)
              (make-paragraph-if-necessary
               (process-text  sectional-unit-buffer
                              #'(lambda(x) (or (is-a
                                                (child-of-sectional-unit
                                                 sectional-unit-name)
                                                (lookat-current-entry
                                                 x))
                                               (end-of-buffer? x))))))
        (when (exists-child-of-sectional-unit? sectional-unit-name )
          (setf (children  new-sectional-unit)
                (get-sectional-units! sectional-unit-buffer
                                      :sectional-unit-name
                                      (child-of-sectional-unit
                                       sectional-unit-name)
                                      )))
        (pop-enclosing-referend)
        new-sectional-unit))))

;;; Function: GET-SECTIONAL-UNITS!                           Author: raman
;;; Created: Thu Apr  9 17:38:48 1992

(defun get-sectional-units! (text-buffer
                             &key(sectional-unit-name 'section))
  "Gets a list of sectional-units.
Default is section.
Leaves the pointer of text buffer pointing at  next unit"
  (mapcar #'(lambda(sectional-unit-text)
              (first (process-text (make-buffer :contents
                                                (list sectional-unit-text))
                                   #'(lambda(x) (end-of-buffer? x))
                                   ))
              )
          (pop-while-true text-buffer
                          #'(lambda(x)
                              (is-a sectional-unit-name  x)))))

(define-parsing-function 'part
    #'(lambda(x )
        (create-sectional-unit  (pop-current-entry x )
                                :sectional-unit-name 'part)))

(define-parsing-function 'chapter
    #'(lambda(x )
        (create-sectional-unit  (pop-current-entry x )
                                :sectional-unit-name 'chapter)))

(define-parsing-function 'section
    #'(lambda(x )
        (create-sectional-unit  (pop-current-entry x )
                                :sectional-unit-name 'section)))

(define-parsing-function 'subsection
    #'(lambda(x )
        (create-sectional-unit  (pop-current-entry x )
                                :sectional-unit-name 'subsection )))

(define-parsing-function 'subsubsection
    #'(lambda(x )
        (create-sectional-unit  (pop-current-entry x )
                                :sectional-unit-name 'subsubsection)))

;;; Function: GENERATE-SECTION-NUMBER                        Author: raman
;;; Created: Tue Sep  1 13:04:05 1992

(defun generate-section-number (parent child)
  "Generate section number of the form parent.child"
  (if (null parent)
      (format nil  "~a" child)
      (format   nil "~a.~a" parent child)))

;;; Function: NUMBER-LIST-SECTIONAL-UNITS                    Author: raman
;;; Created: Tue Sep  1 13:11:02 1992

(defun number-list-sectional-units (list-of-sectional-units
                                    &key (parent nil))
  "Numbers sectional units passed as a list"
  (let ((number 1))
    (dolist
        (sectional-unit list-of-sectional-units)
      (setf
       (sectional-unit-number sectional-unit)
       (generate-section-number parent number))
      (when (children sectional-unit)
        (number-list-sectional-units
         (children sectional-unit)
         :parent  (sectional-unit-number sectional-unit)))
      (incf number))
    list-of-sectional-units))

;;; Function: GET-UNIT-TITLE!                                Author: raman
;;; Created: Fri Nov  1 15:37:36 1991


(defun get-unit-title! (text-buffer)
  "Return first item in buffer and advance pointer if it is a block."
  (process-text-block
   (make-buffer
    :contents
    (list (pop-when-true text-buffer #'(lambda(x) (is-a 'block x)))))))


;;; Function: GET-LABEL!                                     Author: raman
;;; Created: Fri Nov  1 15:40:29 1991

(defun get-label! (text-buffer)
  "Return argument to \label if found and advance pointer "
  (let ((token (lookat-current-entry text-buffer )))
    (cond
      ((and
        (or (is-a 'cs token) (is-a 'math-cs token))
        (or (string=  "label" (cs-name token ))
         (string=  "label" (math-cs-name token ))))
       (advance-pointer text-buffer)
       (label-expand  (pop-current-entry text-buffer ))))))

;;; end of file ;;;
