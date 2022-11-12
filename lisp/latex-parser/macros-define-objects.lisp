;;;   -*-   Mode: LISP -*-    ;;;
 ;;;                                                                       ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Modified: Fri Dec 11 11:00:28 EST 1992
;;; Instead of having a separate slot for each argument, create a
;;; single slot called arguments which contains a list of the
;;; arguments. Provide an accessor method which takes the object being
;;; defined and an integer and returns the argument in the position
;;; specified by integer. Also, define children as an alternative
;;; accessor for the arguments slot.
;;; A <(backup is being kept)> of this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;  tex macros define new objects .
 ;;; refer to the notes on <(macros define objects)>
 ;;; Thu Oct 15 12:58:18 EDT 1992
 ;;; defines the necessary macros to set up things.

 ;;; define class
 ;;; define processing function
(defmacro define-text-object (
                              &key
                              macro-name number-args processing-function
                              children-are-called
                              precedence object-name supers)
  "define new object in text"
  `(progn
     (progn
;;; First define the class:
;;; if n args is 0 no argument slot
       (cond
         (, (= 0 number-args )
            (defclass ,object-name   ,supers
              ((contents :initform nil :initarg  :contents
                         :accessor contents ))
              (:documentation ,(format nil
                                       "Class ~a corresponding to macro ~a"
                                       object-name macro-name))
              ))
                                        ;otherwise
         (t (defclass ,object-name   ,supers
              ((arguments :initform nil :initarg :arguments
                          :accessor arguments :accessor children)
               (children-are-called :initform  ,children-are-called
                                    :initarg :children-are-called
                                    :accessor children-are-called )
               (contents :initform nil :initarg  :contents
                         :accessor contents ))
              (:documentation ,(format nil
                                       "Class ~a corresponding to macro ~a"
                                       object-name macro-name))
              ))
         ))                       ; end if
;;; Now define the processing function:
;;; Modified to match class definition
     (defun ,processing-function (&rest arguments)
       "Automatically generated processing function"
       (assert
        (or (= 0 ,number-args)
            (= (length arguments )  ,number-args))
        nil
        "Wrong number of arguments passed to automatically generated
processing function")
       (let*
           ((self (make-instance ',object-name
                                 :contents  ,macro-name ))
            (processor (if (math-p self)
                           #'process-argument-as-math
                           #'process-argument )))
         (unless (= 0 ,number-args)
           (setf (arguments self)
                 (loop for arg in arguments
                       collect
                       (funcall processor  arg)
                       )))
         self)
       )
;;; define argument accessor method
                                        ; and children-called method
;;; only if n-args is not 0
     (when ,(>  number-args 0)
       (defmethod argument ((,object-name ,object-name) (n integer))
         "Automatically generated argument accessor"
         (assert (<= n (length (arguments  ,object-name ))) nil
                 "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
                 n ,object-name  (length (arguments ,object-name )))
         (elt  (arguments ,object-name)  (- n 1 )))
                                        ;  with arguments in the reverse order
       (defmethod argument ((n integer) (,object-name ,object-name))
         "Automatically generated argument accessor"
         (assert (<= n (length (arguments  ,object-name ))) nil
                 "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
                 n ,object-name  (length (arguments ,object-name )))
         (elt  (arguments ,object-name)  (- n 1 ))
         )
       )                                ; end when
                                        ; children-are-called
     (when ,children-are-called
       (defmethod name-of-child  ((,object-name ,object-name) (n integer))
         "Automatically generated name of child  accessor"
         (assert (<= n (length (arguments  ,object-name ))) nil
                 "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
                 n ,object-name  (length (arguments ,object-name )))
         (cond
           ((null  (children-are-called ,object-name ) ) nil)
           ((listp (children-are-called  ,object-name) )
            (elt  (children-are-called ,object-name)  (- n 1 )))
           ((atom  (children-are-called ,object-name ))
            (children-are-called ,object-name))
           (t (error "Should not have got here. "))
           )
         )
                                        ; with calling sequence reversed
       (defmethod name-of-child  ((n integer) (,object-name ,object-name))
         "Automatically generated name of child  accessor"
         (assert (<= n (length (arguments  ,object-name ))) nil
                 "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
                 n ,object-name  (length (arguments ,object-name )))
         (cond
           ((null  (children-are-called ,object-name ) ) nil)
           ((listp (children-are-called  ,object-name) )
            (elt  (children-are-called ,object-name)  (- n 1 )))
           ((atom  (children-are-called ,object-name ))
            (children-are-called ,object-name))
           (t (error "Should not have got here. "))
           )
         )
       )                                ; end when
;;; define precedence
     (when ',precedence
       (define-precedence ,macro-name :same-as ',precedence))
;;; Install processing function
     (define-tex-macro ,macro-name ,number-args ',processing-function)
     )
  )

;;{{{ labelled text objects:
;;;
;;; Tue May  4 11:21:57 EDT 1993
 ;;;
;;; One class of macros that the define-text-object fails to handle is
;;; the following:
;;;
;;; Macros whose first argument is a label, and the second argument is
;;; the object being labelled.
;;; Gries uses this in his book:
;;;
;;; By default, the label is attached to the enclosing referend. So
;;; the automatically generated parsing function produced by
;;; define-text-object will end up putting the label on the enclosing
;;; section or paragraph, rather than the second argument to the macro
;;; which is actually the object being labelled.
;;; Will try to handle this class of macros using a
;;; define-text-object-with-label
;;; Questions:
;;; The label could be either the first or the second argument,
;;; provide for this with a label-first flag.
;;; The label could be supplied as a label tag or as a call to the
;;; \label macro. Gries uses the latter, but the former would actually
;;; make for less typing, and is as likely.
;;;
;;; Starting with the macro definition for define-text-object
;;; Only anticipate changing the processing function:
;;;
;;; Such macros will only have 2 arguments.
;;;

(defmacro define-text-object-with-label
    (&key
       macro-name  processing-function
       (label-first t)
       precedence object-name supers)
  "define new object in text"
  `(progn
;;; First define the class:
     (defclass ,object-name   ,(append supers
                                (list  'labelled-class 'numbered-class))
       ((arguments :initform nil :initarg :arguments :accessor arguments :accessor children)
        (contents :initform nil :initarg  :contents :accessor contents ))
       (:documentation
        ,(format nil
                 "Class ~a corresponding to
                                   document macro with label  ~a"
                 object-name macro-name)))

;;; Modified:
;;; process label,
;;; then process referend
;;; finally install the label
;;; Now define the processing function:
;;; Modified to match class definition
     (defun ,processing-function (&rest arguments)
       "Automatically generated processing function"
       (assert (= (length arguments )  2) nil
               "Wrong number of arguments
passed to automatically generated processing function")
       (let*
           ((self (make-instance ',object-name
                                 :contents  ,macro-name ))
            (processor (if (math-p self)
                           #'process-argument-as-math
                           #'process-argument )))
;;; First process the object:
         (setf (arguments self)
               (if ,label-first
                   (funcall processor (second arguments ))
                   (funcall processor (first arguments ))))
                                        ; Now make it the referend
         (add-enclosing-referend self)
                                        ; number it
         (when (numbered-class-p self )
           (increment-counter-value  (class-name (class-of self )))
           (setf (anumber self )  (next-counter-value (class-name (class-of self )))))
                                        ; Now process the label,
                                        ;it will automatically point to the referend
         (if ,label-first
             (funcall processor (first arguments ))
             (funcall processor (second arguments )))
         (pop-enclosing-referend)
         self)
       )
;;; define argument accessor method
     (defmethod argument ((,object-name ,object-name) (n integer))
       "Automatically generated argument accessor"
       (assert  (<= n (length (arguments  ,object-name ))) nil
                "Not that many arguments:  n = ~a, but ~a has only ~a arguments. "
                n  ,object-name (length (arguments ,object-name )))
       (elt  (arguments ,object-name)  (- n 1 ))
       )
                                        ; define  arguments in  reverse order
     (defmethod argument ((n integer) (,object-name ,object-name))
       "Automatically generated argument accessor"
       (assert  (<= n (length (arguments  ,object-name ))) nil
                "Not that many arguments:  n = ~a, but ~a has only ~a arguments. "
                n  ,object-name (length (arguments ,object-name )))
       (elt  (arguments ,object-name)  (- n 1 ))
       )
;;; define precedence
     (when ',precedence
       (define-precedence ,macro-name :same-as ',precedence))
;;; Install processing function
     (define-tex-macro ,macro-name 2 ',processing-function)))

;;}}}

;;; default name-of-child methods:

(defmethod name-of-child ((document document) (n integer )) nil)
(defmethod name-of-child ((n integer) (document document )) nil)

  ;;; Method: THIS-ARGUMENT-IS-CALLED                          Author: raman
  ;;; Created: Wed Apr  7 17:32:11 1993

(defmethod this-argument-is-called ((document document))
  "What is this object called where it occurs?"
  (let*  ((this-parent (parent  document ))
          (parent-children (children this-parent))
          (child-position (when
                              (listp parent-children)
                            (position document (children
                                                this-parent )))))
    (when child-position  (incf child-position ))
    (when this-parent
      (name-of-child (or child-position  1) this-parent ))
    )
  )

(defmethod this-argument-is-called ((attribute attribute ))
  "Attributes are not arguments "
  nil)

(defmethod this-attribute-is-called ((document document )) nil)

(defmethod attributes-are-called ((document document )) nil)
(defmethod this-attribute-is-called ((attribute attribute ))
  "What is this attribute called?"
  (let*
      ((parent (parent attribute))
       (call-attributes (attributes-are-called parent ))
       (name-spec (find
                   (attribute-name attribute) call-attributes
                   :key #'car )))
    (when name-spec
      (second name-spec ))
    )
  )
