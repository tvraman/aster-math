;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(in-package :aster)

;;; This  file processes the text occuring in the body of the article.
;;; ie: processes the text occuring inside sections, subsections etc.
;;; This file handles Latex environments, and the next file will specifically
;;; handle mathematical constructs.
;;; The next file also handles control sequences.

;;; Function: PEEL-OFF-LISTS                                 Author: raman
;;; Created: Fri Nov 27 10:49:25 1992

(defun peel-off-lists (nested-list)
  "Peel off unnecessary parens "
  (cond
    ((null nested-list) nested-list)
    ((not (listp nested-list))  nested-list)
    ((and (listp nested-list)
          (> (length nested-list)  1)) nested-list)
    ((and (listp nested-list)
          (not (listp (first nested-list )))) nested-list)
    (t (peel-off-lists (first nested-list)))
    )
  )

;;; Function: PROCESS-TEXT                                   Author: raman
;;; Created: Tue Nov  5 15:47:44 1991

(defun process-text (text-buffer
                     &optional (termination-condition? #'end-of-buffer?))
  "Takes a buffer  containing text, and  processes it until
termination-condition is satisfied.  Upon exit, buffer-pointer points to after processed text"
  (let ((processed-text nil)
        (current-paragraph nil))
    (loop
      for token = (lookat-current-entry text-buffer )
      until (funcall termination-condition?  text-buffer)
      do
         (cond
           ((end-of-buffer? text-buffer)
            (error " Processing text  end reached before close delimiter"))
           ((eq 'parbreak token)
            (advance-pointer  text-buffer)
            (push
             (make-paragraph
              :contents
              (delete nil  (nreverse current-paragraph )))
             processed-text)
            (setf current-paragraph nil))
           (t
            (push (funcall (get-parser token) text-buffer) current-paragraph )))
      finally
         (return
           (peel-off-lists
            (nreverse
             (delete
              nil
              (push
               (if processed-text
                   (make-paragraph
                    :contents
                    (nreverse (delete nil current-paragraph )))
                   (nreverse (delete nil current-paragraph )))
               processed-text ))))))))

;;; Function: PROCESS-WORD                                   Author: raman
;;; Created: Sun Jan 26 15:42:16 1992

(defun process-word (text-buffer )
  "Process a word "
  (pop-current-entry text-buffer))

;;; Function: PROCESS-COMMENT                                Author: raman
;;; Created: Fri Feb 28 10:52:02 1992

(defun process-comment (text-buffer)
  "Process a comment"
  (advance-pointer text-buffer )
  nil)

;;; Function: PROCESS-NEWLINE                                Author: raman
;;; Created: Thu Feb 27 20:55:28 1992

(defun process-newline (text-buffer)
  "process newlines "
  (pop-current-entry text-buffer))

(defun process-field-separator (text-buffer)
  "process field-separators "
  (pop-current-entry text-buffer))
;;; Function: PROCESS-CENTER                                 Author: raman
;;; Created: Sun Jan 26 15:42:23 1992

(defun process-center (text-buffer )
  "process center"
  (let ((new-centered-text (make-centered-text )))
    (setf (contents new-centered-text)
          (process-text (make-buffer :contents (rest (pop-current-entry
                                                      text-buffer )))))
    new-centered-text))

;;; Function: PROCESS-TEXT-BLOCK                                  Author: raman
;;; Created: Wed Nov 13 21:31:01 1991
(defun process-text-block (text-buffer )
  "Process text blocks."
  (let* ((contents nil)
         (current-block (pop-current-entry text-buffer))
         (current-block-buffer (make-buffer :contents (rest current-block))))
    (setf contents
          (delete nil (process-text current-block-buffer)))
    (make-text-block :contents contents
                     :local-env (buffer-local-env current-block-buffer))))

;;; Function: PROCESS-INLINE-QUOTE                           Author: raman
;;; Created: Thu Feb 13 20:03:11 1992

(defun process-inline-quote (text-buffer )
  "process inline quotation"
  (let ((inline-quote-buffer
          (make-buffer :contents (rest (pop-current-entry text-buffer ))))
        (inline-quote nil))
    (setf inline-quote
          (process-text inline-quote-buffer
                        #'(lambda(x)
                            (or (equal "''" (lookat-current-entry x))
                                (end-of-buffer? x)))))
    (if (end-of-buffer? inline-quote-buffer)
        (cons ' mismatched-quote inline-quote)
        (create-QUOTED-TEXT inline-quote :QUOTED-TEXT-TYPE 'inline-quote))))
;;; Function: VALIDATE-QUOTED-TEXT-TYPE                      Author: raman
;;; Created: Mon Apr 13 19:27:21 1992

(defun validate-quoted-text-type (quoted-text-type)
  "validate quoted-text-type"
  (find quoted-text-type '(quote-environment quotation-environment inline-quote))
  )

;;; Function: CREATE-QUOTED-TEXT                             Author: raman
;;; Created: Mon Apr 13 19:32:44 1992

(defun create-quoted-text (quote &key(quoted-text-type 'inline-quote) )
  "Create a quoted text object"
  (let ((new-quoted-text
          (make-quoted-text :quoted-text-type quoted-text-type )))
    (setf (contents new-quoted-text) quote)
    new-quoted-text))

;;; Function: PROCESS-QUOTE                                  Author: raman
;;; Created: Sun Jan 26 15:42:29 1992

(defun process-quote (text-buffer )
  "process   quote"
  (create-quoted-text
   (process-text
    (make-buffer :contents (rest (pop-current-entry text-buffer ))))
   :quoted-text-type 'quote-environment))

  ;;; Function: PROCESS-TEXT-NUMBER                            Author: raman
  ;;; Created: Tue Dec 22 14:19:58 1992

(defun process-text-number (text-buffer)
  "Process a number string in text"
  (make-instance 'text-number
                 :contents (list  (second (pop-current-entry text-buffer )))))

(defun process-math-number (math-buffer)
  "Process a number string in math"
  (make-instance 'math-number
                 :contents (second (pop-current-entry math-buffer ))))

;;; Function: PROCESS-QUOTATION                              Author: raman
;;; Created: Sun Jan 26 15:42:33 1992

(defun process-quotation (text-buffer )
  "process   quotation"
  (create-quoted-text
   (process-text
    (make-buffer :contents (rest (pop-current-entry text-buffer ))))
   :quoted-text-type 'quotation-environment ))

;;; Function: PROCESS-NEW-ENVIRONMENT                        Author: raman
;;; Created: Sat Feb 15 12:40:11 1992

(defun process-new-environment (text-buffer )
  "process an unknown latex environment"
  (let* ((environment-contents(rest  (pop-current-entry text-buffer)))
         (environment-name  (first  environment-contents))
         (new-environment
           (create-new-environment :env-name         environment-name )))
    (when (can-this-be-cross-referenced? 'new-environment )
      (add-enclosing-referend new-environment ))
    (when (numbered-class-p new-environment)
      (setf (numbered-class-number new-environment)
            (increment-counter-value (class-name (class-of new-environment )))))
    (setf (contents new-environment)
          (process-text
           (make-buffer :contents (rest environment-contents))))
    (when (can-this-be-cross-referenced? 'new-environment )
      (pop-enclosing-referend  ))
    new-environment))

;;; Function: PROCESS-INLINE-MATH                            Author: raman
;;; Created: Wed Nov  6 16:11:44 1991

(defun process-inline-math (text-buffer )
  "Process in line math"
  (make-instance 'inline-math
                 :contents
                 (list
                  ( process-math
                    (make-buffer
                     :contents (rest (pop-current-entry text-buffer )))))))

;;; Function: PROCESS-DISPLAY-MATH                           Author: raman
;;; Created: Wed Nov  6 16:12:35 1991

(defun process-display-math (text-buffer )
  "Process display math"
  (make-instance
   'display-math
   :contents
   (list
    (process-math
     (make-buffer :contents (rest (pop-current-entry text-buffer )))))))

;;; Function: PROCESS-CS                                     Author: raman
;;; Created: Tue Nov  5 20:30:06 1991

(defun process-cs (text-buffer )
  "Process cs found in  current position in buffer"
  (expand-tex-macro text-buffer))

  ;;; Function: DEFINED-TEX-MACRO-P                            Author: raman
  ;;; Created: Wed Dec 16 12:17:34 1992

(defun defined-tex-macro-p (macro-name)
  "See if this has been defined as a tex macro"
  (let ((macro-table-entry (get-tex-macro macro-name )))
    (not (equal (tex-macro-name macro-table-entry) 'default ))))

;;; Function: EXPAND-TEX-MACRO                               Author: raman
;;; Created: Thu Feb  6 11:52:43 1992

(defun expand-tex-macro ( text-buffer)
  "process tex macro"
  (let* ((token (pop-current-entry text-buffer ))
         (macro-name(or  (cs-name token )
                         (math-cs-name token )))
         (macro-table-entry (get-tex-macro macro-name ))
         (n-args (tex-macro-number-of-args macro-table-entry)))
    (cond
      (( eq  (tex-macro-name macro-table-entry) 'default)
       (make-tex-defined-macro :tex-defined-macro-name macro-name))
      ((equal "label" (tex-macro-name macro-table-entry ))
       (apply
        (tex-macro-expand macro-table-entry)
        (pop-next-n-entries text-buffer n-args))
       (values)) ;do not return processed label
                                        ; processing label only causes a side-effect
      ((= 0 n-args )
       (funcall (tex-macro-expand macro-table-entry) text-buffer))
      ((>  n-args 0)
       (apply
        (tex-macro-expand macro-table-entry)
        (pop-next-n-entries text-buffer n-args)))
      (t
       (error "~a expects ~a arguments!" macro-name n-args)))))

;;; Function: PROCESS-ARRAY                                  Author: raman
;;; Created: Sun Jan 26 15:26:42 1992

(defun process-array (text-buffer )
  "Process an array "
  (let (( array-contents (rest (pop-current-entry text-buffer))))
    (make-instance
     'math-array
     :contents   (map2-nested-list #'process-array-element array-contents ))))

;;; Function: PROCESS-TABULAR                                Author: raman
;;; Created: Sun Jan 26 15:26:45 1992

(defun process-tabular (text-buffer )
  "Process a table "
  (let ((table-contents (rest (pop-current-entry text-buffer))))
    (make-instance
     'tabular
     :contents  (map2-nested-list #'process-table-element table-contents))))
(defun process-cases (text-buffer )
  "Process a cases environment  "
  (let ((table-contents (rest (pop-current-entry text-buffer))))
    (cons
     'cases
     (map2-nested-list #'process-table-element table-contents))))

;;; Function: VALIDATE-LIST-ENVIRONMENT-TYPE                 Author: raman
;;; Created: Sat Apr 11 17:48:47 1992

(defun validate-list-environment-type (list-environment-type)
  "validate list environment type"
  (or
   (find list-environment-type
         '(enumerated-list itemized-list description-list))
   (error "~a is not a valid list environment type"
          list-environment-type)))

;;; numbering items:

;;; Function: GENERATE-ITEM-MARKER                           Author: raman
;;; Created: Thu Sep  3 18:40:01 1992

(defun generate-item-marker (parent child)
  "generate item marker"
  (if (null parent)
      (format nil  "~a" child)
      (format   nil "~a.~a" parent child)
      )
  )

;;; numbering items works by side-effecting argument.
;;; Function: NUMBER-ITEMS                           Author: raman
;;; Created: Thu Sep  3 18:33:07 1992

(defun number-items (list-of-items &key(parent nil))
  "Number list of items"
  (let ((number 1))
    (dolist
        (item list-of-items)
      (setf  (item-marker item)
             (generate-item-marker  parent
                                    number))
      (mapcar #'(lambda(item ) (number-sub-items-if-necessary
                                item
                                :parent number ))
              (item-contents item))
      (incf number)
      )
    list-of-items
    )
  )

;;; Function: NUMBER-SUB-ITEMS-IF-NECESSARY                  Author: raman
;;; Created: Thu Sep  3 18:47:38 1992

(defun number-sub-items-if-necessary (list-of-text &key(parent nil))
  "Number any sub-lists in text"
  (if  (listp  list-of-text)
       (let ; first disjunct
           ((to-be-numbered  (remove-if-not
                              #'list-environment-p
                              list-of-text)))
         (when to-be-numbered
           (dolist
               (sub-list to-be-numbered)
             (number-list-environment sub-list
                                      :parent parent)
             )
           )
         )
       list-of-text) ;second disjunct
  )

;;; Function: CREATE-LIST-ENVIRONMENT                        Author: raman
;;; Created: Sat Apr 11 17:37:43 1992

(defun create-list-environment (list-of-items
                                &key (list-environment-type 'enumerated-list))
  "Create a list environment of specified type,
default is enumerated list."
  (let ((new-list-environment (make-list-environment :list-environment-type list-environment-type )))
    (setf (items new-list-environment) (remove-null-items list-of-items ))
    (number-list-environment new-list-environment)
    new-list-environment))

  ;;; Function: REMOVE-NULL-ITEM                               Author: raman
  ;;; Created: Sat Jan 30 15:33:46 1993

(defun remove-null-items (list-of-items)
  "Remove null item from list"
  (cond
    ((listp  list-of-items )
     (remove-if
      #'(lambda(item) (and (item-p item) (null (item-contents item ))))
      list-of-items))
    (t list-of-items )))

;;; Function: NUMBER-LIST-ENVIRONMENT                        Author: raman
;;; Created: Thu Sep  3 20:50:08 1992

(defun number-list-environment (list-environment &key (parent nil))
  "number items in a list environment. Only enumerated lists numbered. "
  (when (enumerated-list-p list-environment)
    (number-items (items list-environment)
                          :parent parent)
    list-environment))

;;; Function: PROCESS-ENUMERATE                              Author: raman
;;; Created: Sun Jan 26 15:26:48 1992

(defun process-enumerate (text-buffer )
  "Process enumerated list of items "
  (create-list-environment
   (process-text (make-buffer :contents (rest (pop-current-entry text-buffer ))))
   :list-environment-type 'enumerated-list))

;;; Function: PROCESS-DESCRIPTION                            Author: raman
;;; Created: Sun Jan 26 15:26:51 1992

(defun process-description (text-buffer )
  "process a latex description environment "
  (create-list-environment
   (process-text (make-buffer :contents (rest (pop-current-entry text-buffer ))))
   :list-environment-type 'description-list))

;;; Function: PROCESS-ITEMIZE                                Author: raman
;;; Created: Sun Jan 26 15:26:54 1992

(defun process-itemize (text-buffer )
  "process a latex   itemize environment "
  (create-list-environment
   (process-text (make-buffer :contents (rest (pop-current-entry text-buffer ))))
   :list-environment-type 'itemized-list))

;;; Function: PROCESS-ITEM                                   Author: raman
;;; Created: Sun Jan 26 15:26:57 1992

(defun process-item (text-buffer )
  "process a latex   item environment "
  (let ((new-item (make-item )))
    (add-enclosing-referend new-item)
    (setf (item-contents  new-item)
          (process-text (make-buffer :contents (rest (pop-current-entry text-buffer )))))
    (increment-counter-value 'item)
    (pop-enclosing-referend)
    new-item))

;;; Function: PROCESS-EQUATION                               Author: raman
;;; Created: Sun Jan 26 15:27:00 1992

(defun process-equation (text-buffer )
  "process a latex   equation environment "
  (let* ((contents (rest  (pop-current-entry text-buffer  )))
         (math-equation (make-instance 'math-equation ))
         (equation-buffer  (make-buffer :contents  contents )))
    (add-enclosing-referend math-equation)
    (when (numbered-class-p math-equation )
      (setf (numbered-class-number math-equation )
            (increment-counter-value 'math-equation)))
    (setf  (contents math-equation ) (list  (process-math equation-buffer )))
    (pop-enclosing-referend )
    math-equation))

;;; Function: PROCESS-EQNARRAY                               Author: raman
;;; Created: Sun Jan 26 15:27:03 1992

(defun process-eqnarray (text-buffer )
  "process a latex   eqnarray environment "
  (let (( eqnarray-contents (rest (pop-current-entry text-buffer)))
        (self (make-instance 'math-eqnarray )))
    (add-enclosing-referend self)
    (increment-counter-value 'math-eqnarray)
    (setf (contents self)
          (map2-nested-list #'process-array-element eqnarray-contents))
    (pop-enclosing-referend)
    self))
;;; Function: PROCESS-EQALIGN                                Author: raman
;;; Created: Sat Oct  3 17:14:44 1992

(defun process-eqalign (text-buffer)
  "Process eqalign. "
  (let (( eqalign-contents (rest (pop-current-entry text-buffer))))
    (make-instance
     'math-eqnarray
     :contents
     (map2-nested-list #'process-array-element eqalign-contents))))

;;; Function: PROCESS-SLIDE                                  Author: raman
;;; Created: Mon May  4 12:26:00 1992

(defun process-slide (text-buffer)
  "process a slide"
  (make-slide
   :contents
   (process-text
    (make-buffer :contents (rest (pop-current-entry text-buffer ))))))

;;; Function: PROCESS-VERBATIM                               Author: raman
;;; Created: Fri Aug 28 14:32:14 1992

(defun process-verbatim (text-buffer)
  "Process a latex verbatim environment"
  (make-verbatim
   :contents
   (process-text
    (make-buffer :contents (rest (pop-current-entry text-buffer ))))))

;;; Function: BLOCK-CONTENTS                                    Author: raman
;;; Created: Thu Nov  7 16:24:15 1991

(defun block-contents (item)
  "returns  all but first element of item which is assumed to be a block marked as such by its first element."
  (when (is-a 'block item)
    (rest item)))

;;; Function: PROCESS-ARRAY-ELEMENT                            Author: raman
;;; Created: Tue Jan 28 15:20:14 1992

(defun process-array-element (element)
  "process element by passing to process-math"
  (make-instance
   'table-element
   :contents
   (when element (process-math (make-buffer :contents element )))))

;;; Function: PROCESS-TABLE-ELEMENT                          Author: raman
;;; Created: Tue Jan 28 15:43:54 1992

(defun process-table-element (element)
  "process a table element"
  (make-instance
   'table-element
   :contents
   (process-text (make-buffer :contents element ))))

  ;;; Function: VOID-LIST-P                                    Author: raman
  ;;; Created: Wed Sep 15 21:15:18 1993
(defun void-list-p (list-l)
  "Is this a void list, ie (nil)?"
  (and(listp list-l)
      (= 1 (length list-l))
      (null (first list-l ))))

;;; Function: MAP2-NESTED-LIST                               Author: raman
;;; Created: Tue Jan 28 14:17:42 1992

(defun MAP2-NESTED-LIST(modifier nested-list)
  "map down a nested list"
  (mapcar
   #'(lambda(simple-list)
       (mapcar modifier simple-list))
   (remove-if #'void-list-p nested-list)))

;;; Function: PROCESS-UNKNOWN-CONSTRUCT                      Author: raman
;;; Created: Sat Feb 29 13:17:49 1992

(defun process-unknown-construct (text-buffer)
  "Process an unknown construct, ie just stick it in"
  (cons 'unknown-construct (pop-current-entry text-buffer)))

;;; Function: IS-A                                           Author: raman
;;; Created: Sat Oct 12 13:07:51 1991
(defun is-a (doc-unit lispified-text)
  "Checks if lispified-text is a doc-unit."
  (cond
    ((atom lispified-text)nil)
    ((eq (first lispified-text) doc-unit) lispified-text)))

;;; Function: CS-NAME                                        Author: raman
;;; Created: Mon Nov  4 09:21:55 1991

(defun  cs-name (token)
  "Return name of cs where cs is a list of two elements, the marker 'cs and the name of the cs."
  (when (is-a 'cs token)
    (second token)))

;;; Function: MATH-CS-NAME                                   Author: raman
;;; Created: Tue Mar  3 22:03:26 1992

(defun math-cs-name (token)
  "return name of math cs"
  (when (is-a 'math-cs token)
    (second token)))

;;; end of file
