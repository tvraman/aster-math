;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

#+clisp (use-package :clos)
#+lucid (use-package :clos)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; Modified: Thu Dec 24 14:08:54 EST 1992
;;; Adding extra accessors for slots
;;; accessors like article-title etc could just be title, for the
;;; present defining multiple accessors to avoid making minimal
;;; changes to working code. 
;;; Modified: Thu Dec 24 11:06:39 EST 1992
;;; Modifying predicates like <class>-p to use typep instead of eq
;;; class-name Note: this means that <class>-p subclass is true

;;; Modified: Mon Aug  3 14:33:52 EDT 1992
;;; Removing duplicate definition of math class.
;;; Modified: Thu Apr  2 11:02:47 EST 1992
;;; Introduces classes for document components.
;;; Will completely replace the structures for document components
;;; defined in structs.lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Class: DOCUMENT                                          Author: raman
;;; Created: Thu Apr  2 11:05:22 1992
;;; Modified: Mon Mar 29 08:51:05 EST 1993
;;; added read-next and read-previous slots.
;;; afl-state added before this.
(defclass document ()
  ((parent :initform nil   :initarg :parent :accessor parent)
   (next :initform  nil  :initarg :next :accessor next)
   (previous :initform nil :initarg :previous :accessor previous)
   (weight  :initform nil :initarg weight
            :accessor internal-weight)
   (afl-state :initform nil :initarg :afl-state :accessor afl-state )
   )
  (:documentation "The base class for documents."))

;;; default methods for next-read and previous-read:
(defmethod next-read ((object t)) 'undefined)
(defmethod previous-read ((object t)) 'undefined)
(defun make-document ()
  (let ((self (make-instance 'document)))
    self))

(defun document-p (self)
  (typep self  'document))


;;; Default print-object method for all document objects. Solution
;;; given by smh@franz.com
;;; Modified: Mon Dec 21 16:33:55 EST 1992
;;; Original method gets into an infinite loop  if we have parent slot
;;; so fixing it. Original function is in <(test file)>
;;; Modified: Tue Dec 22 14:56:35 EST 1992
;;; smh suggested setting *print-circle*
;;; seems to work, so setting it in lisp-init.lisp  rather
;;; than just kluging the parent slot.
;;; Modified: Wed Mar 31 12:53:02 EST 1993
;;; added ignore slots

(defparameter  *print-document-object-ignore-slots*
  (list'afl-state  'next-read 'previous-read )
  "slots to ignore while printing document object")
#+(or lucid pcl)
(defmethod print-object ((x document) s)
  (flet
      ((ignore-slots (slots)
         (loop for slot in slots 
               unless (find (slot-definition-name slot) 
                            *print-document-object-ignore-slots* )
               collect slot )))
    (let* ((class (class-of x))
           (slots(ignore-slots  (class-slots class)))
           (l (mapcar #'(lambda (slot)
                          (let* ((name (slot-definition-name slot))
                                 (boundp (slot-boundp x name))
                                 (val (and boundp
                                           (slot-value x name))))
                            (list name boundp val)))
                      slots)))
      (format s "[[~s~%~:{ ~s:~20t~:[<unbound>~;~s~]~%~}]]"
              (class-name class) l))))


  ;;; Method: CHILD                                            Author: raman
  ;;; Created: Thu Sep 23 08:55:52 1993

(defmethod child ((document document)(position integer) )
  "Return nth child of document if any"
  (cond
    ((eq 'undefined (children document)) 'undefined)
    ((not (listp (children document ))) 'undefined)
    ((> position (length (children document ))) 'undefined)
    (t (elt  (children document) position )))
  )

(defmethod child ((position integer)(document document) )
  "Return nth child of document if any"
  (cond
    ((eq 'undefined (children document)) 'undefined)
    ((not (listp (children document ))) 'undefined)
    ((> position (length (children document ))) 'undefined)
    (t (elt  (children document) position )))
  )
;;; The simplest list of objects is a list,
;;; children of a list takes you to the first element?
;;; check:
(defmethod children ((list list )) (first list ))
;;; Class: ARTICLE                                           Author: raman
;;; Created: Thu Apr  2 11:05:54 1992

(defclass article (document)
  ((title :initform nil :initarg :title :accessor article-title
          :accessor title )
   (author :initform nil :initarg :author :accessor article-author
           :accessor author )
   (address :initform nil :initarg :address :accessor article-address)
   (date :initform nil :initarg :date :accessor article-date
         :accessor date )
   (abstract :initform nil :initarg :abstract :accessor
             article-abstract
             :accessor  abstract )
   (time-to-read :initform nil :initarg :time-to-read
                 :accessor internal-time-to-read )
   (initial-body
    :initform nil :initarg :initial-body :accessor
    article-initial-body :accessor contents 
    :accessor initial-body )
   (sectional-units :initform nil :initarg :sectional-units
                    :accessor article-sectional-units
                    :accessor sectional-units  :accessor children )
   (references :initform nil :initarg :references :accessor article-references
               :accessor references ))
  (:documentation "A document belonging to the article class"))

(defun make-article ()
  (let ((self (make-instance 'article)))
    self))

(defun article-p (self)
  (typep self  'article))




;;; Class: SECTIONAL-UNIT                                    Author: raman
;;; Created: Thu Apr  2 11:45:16 1992
;;; Section body is its contents, subsections are its children. 
(defclass sectional-unit (document)
  ((name :initform nil :initarg :name :accessor sectional-unit-name)
   (title :initform nil :initarg :title :accessor sectional-unit-title
          :accessor  title )
   (time-to-read :initform nil :initarg :time-to-read
                 :accessor internal-time-to-read )
   (number  :initform nil :initarg :number :accessor
            sectional-unit-number)
   (body :initform nil :initarg :body :accessor sectional-unit-body
         :accessor  body  :accessor contents )
   (sectional-units :initform nil :initarg :sectional-units
                    :accessor sectional-unit-sectional-units
                    :accessor sectional-units 
                    :accessor children))
  (:documentation "A sectional unit, such as section, subsection etc."))

;;; Made more intelligent by adding keyword  argument :unit-name
(defun make-sectional-unit (&key(sectional-unit-name 'sectional-unit))
  (let ((self  nil))
    (when (validate-sectional-unit-name sectional-unit-name)
      (setf self 
	    (make-instance sectional-unit-name))
      )
    self))

(defun sectional-unit-p (self)
  (typep self  'sectional-unit))

  ;;; Class: PART                                              Author: raman
  ;;; Created: Thu Sep 23 13:37:45 1993

(defclass part (sectional-unit)
  ((name :initform 'part  :initarg :name
         :accessor part-name :allocation :class))
  (:documentation "A part"))

(defun make-part ()
  (let ((self (make-instance 'part)))
    self))

(proclaim '(inline part-p))

(defun part-p (self)
  (typep  self 'part))



  ;;; Class: CHAPTER                                           Author: raman
  ;;; Created: Sun Mar  7 11:38:42 1993

(defclass chapter (sectional-unit)
  ((name :initform 'chapter  :initarg :name :reader chapter-name
	 :allocation :class))
  (:documentation "A chapter"))

(defun make-chapter ()
  (let ((self (make-instance 'chapter)))
    self))

(proclaim '(inline chapter-p))

(defun chapter-p (self)
  (eq (class-name (class-of self)) 'chapter))

(defun chapter-subtype-p (self)
  (typep  self 'chapter))


;;; Class: SECTION                                           Author: raman
;;; Created: Thu Apr  2 12:47:32 1992

(defclass section (sectional-unit)
  ((name :initform 'section  :initarg :name :reader section-name
	 :allocation :class))
  (:documentation "a section in an article"))

(defun make-section ()
  (let ((self (make-instance 'section)))
    self))

(defun section-p (self)
  (eq (class-name (class-of self)) 'section))



;;; Class: SUBSECTION                                        Author: raman
;;; Created: Thu Apr  2 12:48:47 1992

(defclass subsection (sectional-unit)
  ((name :initform 'subsection  :initarg :name :reader subsection-name
	 :allocation :class))
  (:documentation "A subsection in an article"))

(defun make-subsection ()
  (let ((self (make-instance 'subsection)))
    self))

(defun subsection-p (self)
  (eq (class-name (class-of self)) 'subsection))



;;; Class: SUBSUBSECTION                                     Author: raman
;;; Created: Thu Apr  2 12:50:20 1992

(defclass subsubsection (sectional-unit)
  ((name :initform 'subsubsection  :initarg :name :reader subsubsection-name
	 :allocation :class))
  (:documentation "a subsubsection in an article"))

(defun make-subsubsection ()
  (let ((self (make-instance 'subsubsection )))
    self))

(defun subsubsection-p (self)
  (eq (class-name (class-of self)) 'subsubsection))


;;; Class: PARAGRAPH                                         Author: raman
;;; Created: Thu Apr  2 11:49:01 1992

(defclass paragraph (document)
  ((contents :initform nil :initarg :contents
             :accessor paragraph-contents :accessor contents
             :accessor  children ))
  (:documentation "A paragraph of text."))

(defun make-paragraph (&key(contents nil))
  (let ((self
	 (if contents 
             (make-instance
              'paragraph
              :contents contents )
             nil)))
    self))

(defun paragraph-p (self)
  (eq (class-name (class-of self)) 'paragraph))



;;; Class: TEXT-BLOCK                                        Author: raman
;;; Created: Thu Apr  2 15:04:11 1992

(defclass text-block (document)
  ((contents :initform nil :initarg :contents :accessor
             text-block-contents
             :accessor  contents )
   (local-environment
    :initform nil
    :initarg :local-environment :accessor
    text-block-local-environment))
  (:documentation "A block of text with its own local bindings"))

(defun make-text-block (&key (contents nil) (local-environment nil))
  (let ((self (make-instance 'text-block
			     :contents contents
			     :local-environment local-environment
			     )))
    self))

(defun text-block-p (self)
  (typep self  'text-block))




;;; Class definitions for latex environments.



;;; Class: ABSTRACT                                          Author: raman
;;; Created: Sat Apr 11 17:08:28 1992

(defclass abstract (document)
  ((contents :initform nil :initarg :contents :accessor
             abstract-contents
             :accessor contents ))
  (:documentation "The abstract of a document"))

(defun make-abstract ()
  (let ((self (make-instance 'abstract)))
    self))

(defun abstract-p (self)
  (eq (class-name (class-of self)) 'abstract))


;;; Class: LIST-ENVIRONMENT                                  Author: raman
;;; Created: Sat Apr 11 17:09:41 1992

(defclass list-environment (document)
  ((items :initform nil :initarg :items :accessor
          list-environment-items
          :accessor items :accessor children ))
  (:documentation "Super class for all list environments such as
enumerated and itemized lists."))

(defun make-list-environment (&key (list-environment-type 'enumerated-list))
  (let ((self   nil))
    (when (validate-list-environment-type list-environment-type)
      (setf self (make-instance list-environment-type))
      )
    self))

(defun  list-environment-p (self)
  (typep self 'list-environment)
  )

;;; enumerated lists,
;;; itemized lists,
;;; description lists,
;;; all inherit from list-environment.

;;; Class: ENUMERATED-LIST                                   Author: raman
;;; Created: Sat Apr 11 17:11:47 1992

(defclass enumerated-list (list-environment )
  ((type :initform 'enumerated-list
	 :initarg :type
	 :reader enumerated-list-type)
   (label :initform nil :initarg :label :accessor label))
  (:documentation "An enumerated list"))

(defun make-enumerated-list ()
  (let ((self (make-instance 'enumerated-list)))
    self))

(defun enumerated-list-p (self)
  (eq (class-name (class-of self)) 'enumerated-list))



;;; Class: ITEMIZED-LIST                                   Author: raman
;;; Created: Sat Apr 11 17:11:47 1992

(defclass itemized-list (list-environment)
  ((type :initform 'itemized-list
	 :initarg :type
	 :reader itemized-list-type))
  (:documentation "An itemized list"))

(defun make-itemized-list ()
  (let ((self (make-instance 'itemized-list)))
    self))

(defun itemized-list-p (self)
  (eq (class-name (class-of self)) 'itemized-list))


;;; Class: DESCRIPTION-LIST                                   Author: raman
;;; Created: Sat Apr 11 17:11:47 1992

(defclass description-list (list-environment)
  ((type :initform 'description-list
	 :initarg :type
	 :reader description-list-type))
  (:documentation "An description list"))

(defun make-description-list ()
  (let ((self (make-instance 'description-list)))
    self))

(defun description-list-p (self)
  (eq (class-name (class-of self)) 'description-list))



;;; Class: ITEM                                              Author: raman
;;; Created: Sat Apr 11 17:25:00 1992

  ;;; Class: NUMBERED-CLASS-MIXIN                              Author: raman
  ;;; Created: Mon May  3 18:33:33 1993

(defclass numbered-class ()
  ((number :initform nil :initarg :number :accessor
           numbered-class-number))
  (:documentation "Mixin class, makes things numbered. "))

(defun make-numbered-class ()
  (let ((self (make-instance 'numbered-class)))
    self))

(proclaim '(inline numbered-class-p))

(defun numbered-class-p (self)
  (typep self 'numbered-class))




  ;;; Class: LABELLED-CLASS                                    Author: raman
  ;;; Created: Mon May  3 18:36:54 1993

(defclass labelled-class ()
  ((label :initform nil :initarg :label :accessor labelled-class-label
          :accessor label))
  (:documentation "Mixin class for making things labelled. "))

(defun make-labelled-class ()
  (let ((self (make-instance 'labelled-class)))
    self))

(proclaim '(inline labelled-class-p))
(defun labelled-class-p (self)
  (typep  self 'labelled-class))




(defclass item (document labelled-class numbered-class)
  ((marker :initform nil :initarg :marker :accessor item-marker)
   (label :initform nil :initarg :label :accessor label)
   (contents :initform nil :initarg :contents :accessor item-contents
             :accessor contents ))
  (:documentation "An item of text "))

(defun make-item ()
  (let ((self (make-instance 'item)))
    self))

(defun item-p (self)
  (eq (class-name (class-of self)) 'item))

;;; Class: CENTERED-TEXT                                     Author: raman
;;; Created: Sat Apr 11 17:17:01 1992

(defclass centered-text (document)
  ((contents :initform nil :initarg :contents
	     :accessor centered-text-contents
             :accessor contents ))
  (:documentation "Centered text"))

(defun make-centered-text (&key (contents nil))
  (let ((self (make-instance 'centered-text
			     :contents contents )))
    self))

(defun centered-text-p (self)
  (eq (class-name (class-of self)) 'centered-text))



;;; Class: QUOTED-TEXT                                       Author: raman
;;; Created: Sat Apr 11 17:17:35 1992

(defclass quoted-text (document)
  ((contents :initform nil :initarg :contents
             :accessor quoted-text-contents
             :accessor contents ))
  (:documentation "quotations "))

(defun make-quoted-text (&key(quoted-text-type 'inline-quote))
  (let
      ((self nil ))
    (when (validate-quoted-text-type quoted-text-type)
      (setf  self (make-instance quoted-text-type )))
    self)
  )


(defun quoted-text-p (self)
  (typep self  'quoted-text))

;;; Class: QUOTE-ENVIRONMENT                                 Author: raman
;;; Created: Sat Apr 11 17:18:17 1992

(defclass quote-environment (quoted-text)
  ((type :initform 'quote :initarg :type :reader quote-environment-type))
  (:documentation "the quote environment of latex"))

(defun make-quote-environment ()
  (let ((self (make-instance 'quote-environment)))
    self))

(defun quote-environment-p (self)
  (eq (class-name (class-of self)) 'quote-environment))

;;; Class: QUOTATION-ENVIRONMENT                                 Author: raman
;;; Created: Sat Apr 11 17:18:17 1992

(defclass quotation-environment (quoted-text)
  ((type :initform 'quotation
	 :initarg :type
	 :reader quotation-environment-type))
  (:documentation "the quotation environment of latex"))

(defun make-quotation-environment ()
  (let ((self (make-instance 'quotation-environment)))
    self))

(defun quotation-environment-p (self)
  (eq (class-name (class-of self)) 'quotation-environment))



;;; Class: INLINE-QUOTE                                      Author: raman
;;; Created: Mon Apr 13 19:19:50 1992

(defclass inline-quote (quoted-text)
  ((type :initform 'inline-quote  :initarg :type :accessor
         inline-quote-type))
  (:documentation "in-line quote."))

(defun make-inline-quote ()
  (let ((self (make-instance 'inline-quote)))
    self))

(defun inline-quote-p (self)
  (eq (class-name (class-of self)) 'inline-quote))


;;; Class: COMMENT                                           Author: raman
;;; Created: Thu Apr 16 19:12:24 1992

(defclass comment (document)
  ((contents :initform nil :initarg :contents :accessor
             comment-contents
             :accessor contents ))
  (:documentation "A comment in the typesetting source. "))

(defun make-comment (&key(contents nil))
  (let ((self (make-instance 'comment
			     :contents contents)))
    self))

(defun comment-p (self)
  (eq (class-name (class-of self)) 'comment))



;;; Class: NEW-ENVIRONMENT                                   Author: raman
;;; Created: Thu Apr 16 19:26:46 1992

(defclass new-environment (document labelled-class numbered-class)
  ((name :initform nil :initarg :name :accessor new-environment-name
         :accessor name )
                                        ;   (label :initform nil :initarg :label :accessor label)
   (contents :initform nil :initarg :contents :accessor
             new-environment-contents
             :accessor contents ))
  (:documentation "A new environment not previously handled."))

;;; default label method:

  ;;; Method: LABEL                                            Author: raman
  ;;; Created: Fri Apr 30 11:11:41 1993

(defmethod label ((object t))
  "This cannot be labelled"
  'undefined)

(defun make-new-environment (&key(name nil) (contents nil))
  (let ((self (make-instance 'new-environment
			     :name name
			     :contents contents )))
    self))

(defun new-environment-p (self)
  (typep self  'new-environment))



  ;;; Variable: *LATEX-NEW-ENVIRONMENT-NAMES*                  Author: raman
  ;;; Created: Wed Feb 24 09:46:02 1993

(defvar *latex-new-environment-names*
  (make-hash-table :test #'equal)
  "List of new latex environments")

  ;;; Macro: DEFINE-NEW-ENVIRONMENT                            Author: raman
  ;;; Created: Wed Feb 24 09:41:56 1993
;;; If keyword supers defined then this should be a subclass of new-environment
(defmacro define-new-environment (&key env-name class-name
                                       supers)
  "Define a new environment of this name"
  `(progn
    (setf (gethash ,env-name *latex-new-environment-names*)
     ',class-name)
    (cond
     (',supers
        (defclass ,class-name ,supers ()
                  (:documentation
                   ,(format nil "Latex environment ~a a subclass of new-environment."
                           class-name))))
        (t (defclass ,class-name (new-environment) ()
                  (:documentation
                   ,(format nil "Latex environment ~a a subclass of new-environment."
                           class-name))))
        )                               ; end if supers 
                                        ; all new environments in latex can be cross-referenced.
    (push ',class-name *objects-that-can-be-referred*)
    (values))
  )

  ;;; Function: CREAATE-NEW-ENVIRONMENT                        Author: raman
  ;;; Created: Wed Feb 24 09:51:38 1993

(defun create-new-environment (&key env-name) 
  "Create an instance of a user defined  new latex environment"
  (assert env-name nil "env-name should be non nil")
  (let ((class-name (gethash  env-name *latex-new-environment-names* )))
    (if class-name
        (make-instance class-name :name env-name)
        (make-instance 'new-environment  :name env-name ))
    )
  )

;;; Class: TEX-DEFINED-MACRO                                 Author: raman
;;; Created: Tue Apr 28 16:43:15 1992

(defclass tex-defined-macro (document)
  ((name :initform nil :initarg :name :accessor tex-defined-macro-name)
   (read-as :initform nil :initarg :read-as
	    :accessor tex-defined-macro-read-as)
   )
  (:documentation "a tex macro."))

(defun make-tex-defined-macro (&key (tex-defined-macro-name nil)
				    (read-as nil))
  (let ((self (make-instance 'tex-defined-macro
			     :name tex-defined-macro-name
			     :read-as read-as )))
    self))

(defun tex-defined-macro-p (self)
  (eq (class-name (class-of self)) 'tex-defined-macro))

;;; Class: CITATION                                          Author: raman
;;; Created: Wed Apr 29 15:24:45 1992

(defclass citation (document)
  ((label :initform nil :initarg :label :accessor citation-label
          :accessor label ))
  (:documentation "A citation in a document."))

(defun make-citation (&key (label nil))
  (let ((self (make-instance 'citation
			     :label label )))
    self))

(defun citation-p (self)
  (eq (class-name (class-of self)) 'citation))


;;; Class: FOOTNOTE                                          Author: raman
;;; Created: Wed Apr 29 19:52:14 1992

(defclass footnote (document)
  ((label :initform nil :initarg :label :accessor footnote-label
          :accessor label )
   (text :initform nil :initarg :text :accessor footnote-text
         :accessor contents ))
  (:documentation "a footnote"))

(defun make-footnote (&key (label  nil) (text nil))
  (let ((self (make-instance 'footnote
			     :label label
			     :text text )))
    self))

(defun footnote-p (self)
  (eq (class-name (class-of self)) 'footnote))




;;; Class: INDEX-TERM                                        Author: raman
;;; Created: Mon Oct  5 11:47:40 1992

(defclass index-term (document)
  ((contents :initform nil :initarg :contents :accessor
             index-term-contents
             :accessor contents ))
  (:documentation "An index term"))

(defun make-index-term ()
  (let ((self (make-instance 'index-term)))
    self))

(proclaim '(inline index-term-p))

(defun index-term-p (self)
  (eq (class-name (class-of self)) 'index-term))


;;; Class: SLIDE                                             Author: raman
;;; Created: Mon May  4 12:29:13 1992

(defclass slide (document)
  ((contents :initform nil :initarg :contents
             :accessor slide-contents :accessor children 
             :accessor contents ))
  (:documentation "A slide"))

(defun make-slide (&key (contents nil))
  (let ((self (make-instance 'slide
			     :contents contents )))
    self))

(defun slide-p (self)
  (eq (class-name (class-of self)) 'slide))



;;; Class: VERBATIM                                          Author: raman
;;; Created: Fri Aug 28 14:37:08 1992

(defclass verbatim (document)
  ((contents :initform nil :initarg :contents
             :accessor verbatim-contents
             :accessor contents ))
  (:documentation "A verbatim environment"))

(defun make-verbatim (&key (contents nil))
  (let ((self (make-instance 'verbatim
			     :contents contents )))
    self))


(defun verbatim-p (self)
  (eq (class-name (class-of self)) 'verbatim))


  ;;; Class: TEXT-NUMBER                                       Author: raman
  ;;; Created: Tue Dec 22 14:27:24 1992

(defclass text-number (document)
  ((contents :initform nil :initarg :contents :accessor contents))
  (:documentation "A text number"))

(defun make-text-number ()
  (let ((self (make-instance 'text-number)))
    self))

(proclaim '(inline text-number-p))

(defun text-number-p (self)
  (eq (class-name (class-of self)) 'text-number))


;;; Modified: Fri Oct 30 09:16:00 EST 1992
;;; Math is a super class for all math objects and should not have any
;;; slots.
;;; Removing these

;;; Class: MATH                                              Author: raman
;;; Created: Mon May  4 14:41:12 1992

(defclass math (document)
  (   )
  (:documentation "parent class for math mode."))

(defun make-math ()
  (let ((self (make-instance 'math
                             )))
    self))
(defun math-p (self)
  (typep self 'math))




;;; Class: DISPLAY-MATH                                      Author: raman
;;; Created: Fri Oct 30 09:48:45 1992

(defclass display-math (math)
  ((contents :initform nil :initarg :contents :accessor
             display-math-contents :accessor children
             :accessor contents ))
  (:documentation "Display math"))

(defun make-display-math ()
  (let ((self (make-instance 'display-math)))
    self))

(proclaim '(inline display-math-p))

(defun display-math-p (self)
  (typep self 'display-math))





;;; Class: INLINE-MATH                                       Author: raman
;;; Created: Fri Oct 30 09:49:14 1992

(defclass inline-math (math)
  ((contents :initform nil :initarg :contents :accessor
             inline-math-contents
             :accessor children :accessor contents ))
  (:documentation "Inline math"))

(defun make-inline-math ()
  (let ((self (make-instance 'inline-math)))
    self))

(proclaim '(inline inline-math-p))

(defun inline-math-p (self)
  (typep self 'inline-math))



  ;;; Class: TABLE-MIX-IN                                      Author: raman
  ;;; Created: Tue Jan 12 14:45:57 1993

(defclass table-mix-in  (document)
  ()
  (:documentation "A mixin class for tabular and array"))

(defun make-table-mix-in  ()
  (let ((self (make-instance 'table-mix-in )))
    self))

(proclaim '(inline table-mix-in-p))
(defun table-mix-in-p (self)
  (typep  self 'table-mix-in ))


;;; Class: TABULAR                                           Author: raman
;;; Created: Thu Nov 19 15:25:29 1992
;;; Modified: Tue Jan 12 14:46:51 EST 1993
;;; introduced a mixin class for tabular and math-array
;;; Useful for defining a common link method

(defclass tabular (table-mix-in)
  ((contents :initform nil :initarg :contents :accessor contents
             :accessor children))
  (:documentation "A table in a document"))

(defun make-tabular ()
  (let ((self (make-instance 'tabular)))
    self))

(proclaim '(inline tabular-p))

(defun tabular-p (self)
  (eq (class-name (class-of self)) 'tabular))



  ;;; Class: TABLE-ELEMENT                                     Author: raman
  ;;; Created: Tue Jan 12 13:49:13 1993

(defclass table-element (document)
  ((above :initform nil :initarg :above :accessor table-element-above)
   (below :initform nil :initarg :below :accessor table-element-below)
   (previous :initform nil :initarg :previous :accessor previous
             :accessor table-element-left )
   (next :initform nil :initarg :next  :accessor next
         :accessor table-element-right )
   (contents :initform nil :initarg :contents :accessor table-element-contents
             :accessor contents ))
  (:documentation "A table element"))

(defun make-table-element ()
  (let ((self (make-instance 'table-element)))
    self))

(proclaim '(inline table-element-p))
(defun table-element-p (self)
  (typep  self 'table-element))


;;; Class: MATH-ARRAY                                        Author: raman
;;; Created: Fri Oct 30 11:37:31 1992

(defclass math-array (table-mix-in math)
  ((contents :initform nil :initarg :contents
             :accessor math-array-contents
             :accessor contents
             :accessor children))
  (:documentation "A math array"))

(defun make-math-array ()
  (let ((self (make-instance 'math-array)))
    self))

(proclaim '(inline math-array-p))

(defun math-array-p (self)
  (eq (class-name (class-of self)) 'math-array))


;;; Class: MATH-EQUATION                                     Author: raman
;;; Created: Fri Oct 30 11:39:18 1992

(defclass math-equation (display-math numbered-class labelled-class )
  ((contents :initform nil :initarg :contents :accessor contents)
                                        ;   (label :initform nil :initarg :label :accessor label)
   )
  (:documentation "A math equation"))

(defun make-math-equation ()
  (let ((self (make-instance 'math-equation)))
    self))

(proclaim '(inline math-equation-p))

(defun math-equation-p (self)
  (eq (class-name (class-of self)) 'math-equation))



;;; Class: MATH-EQNARRAY                                     Author: raman
;;; Created: Fri Oct 30 11:40:25 1992

(defclass math-eqnarray (table-mix-in math labelled-class numbered-class)
  ((contents :initform nil :initarg :contents 
             :accessor math-eqnarray-contents
             :accessor contents ))
  (:documentation "An equation array"))

(defun make-math-eqnarray ()
  (let ((self (make-instance 'math-eqnarray)))
    self))

(proclaim '(inline math-eqnarray-p))

(defun math-eqnarray-p (self)
  (eq (class-name (class-of self)) 'math-eqnarray))






  ;;; Class: WORD                                              Author: raman
  ;;; Created: Sat Dec 26 07:19:58 1992

(defclass a-word (document)
  ((contents :initform nil :initarg :contents
             :accessor word-contents :accessor contents))
  (:documentation "A word"))

(defun make-word ()
  (let ((self (make-instance 'word)))
    self))

(proclaim '(inline word-p))

(defun a-word-p (self)
  (typep self 'a-word))

(defun word-subtype-p (self)
  (typep  self 'a-word))




  ;;; Class: LABEL                                             Author: raman
  ;;; Created: Mon Dec 28 14:12:49 1992

(defclass label (document)
  ((contents :initform nil :initarg :contents :accessor contents)
   (name :initform nil :initarg :name :accessor label-name )
   (parent  :initform nil :initarg :parent
            :accessor parent :accessor points-to))
  (:documentation "A label tag"))

;;; label's parent points to object being labelled:


(defun make-label ()
  (let ((self (make-instance 'label)))
    self))

(proclaim '(inline label-p))

(defun label-p (self)
  (typep  self 'label))



  ;;; Class: CROSS-REF                                         Author: raman
  ;;; Created: Mon Dec 28 15:04:08 1992

(defclass cross-ref (document)
  ((children  :initform nil :initarg :children  :accessor children))
  (:documentation "A cross reference"))

(defun make-cross-ref ()
  (let ((self (make-instance 'cross-ref)))
    self))

(proclaim '(inline cross-ref-p))

(defun cross-ref-p (self)
  (typep  self 'cross-ref))



  ;;; Class: DEFINED-TEXT-OBJECT-WITH-LABEL                    Author: raman
  ;;; Created: Tue May  4 13:45:27 1993

(defclass defined-text-object-with-label (document)
  ()
  (:documentation "A mixin class which is a superclass for all objects generated by define-text-object-with-label"))

(defun make-defined-text-object-with-label ()
  (let ((self (make-instance 'defined-text-object-with-label)))
    self))

(proclaim '(inline defined-text-object-with-label-p))

(defun defined-text-object-with-label-p (self)
  (typep  self 'defined-text-object-with-label))

