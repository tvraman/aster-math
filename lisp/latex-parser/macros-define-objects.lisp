;;;   -*-   Mode: LISP -*-    ;;;
  
  

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;


(in-package :aster)
 
;;;  tex macros define new objects .
;;; refer to the notes on <(macros define objects)>
;;; Thu Oct 15 12:58:18 EDT 1992
;;; defines the necessary macros to set up things.

;;; define class
;;; define processing function
(defmacro define-text-object (&key
                                macro-name number-args processing-function
                                children-are-called
                                precedence object-name supers)
  "define new object in text"
  `(let   (                      ; (*muffled-warnings* 'style-warning)
           )
     (progn
;;; First define the class:
;;; if n args is 0 no argument slot
       (cond
         (, (= 0 number-args )
            (defclass ,object-name   ,supers
              ((contents :initform nil :initarg  :contents
                         :allocation :class :accessor contents ))
              (:documentation
               ,(format nil
                        "Class ~a corresponding to macro ~a"
                        object-name macro-name))))
         (t
          (defclass ,object-name   ,supers
            ((arguments :initform nil :initarg :arguments
                        :accessor arguments :accessor children)
             (children-are-called
              :initform  ,children-are-called :allocation :class
              :initarg :children-are-called
              :accessor children-are-called )
             (contents :initform nil :initarg  :contents
                       :allocation :class :accessor contents ))
            (:documentation
             ,(format nil
                      "Class ~a corresponding to macro ~a"
                      object-name macro-name)))))                      
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
              (processor
                (if (math-p self)
                    #'process-argument-as-math
                    #'process-argument )))
           (unless (= 0 ,number-args)
             (setf (arguments self)
                   (loop for arg in arguments
                         collect
                         (funcall processor  arg)
                         )))
           self))
;;; define precedence
       (when ',precedence
         (define-precedence ,macro-name :same-as ',precedence))
;;; Install processing function
       (define-tex-macro ,macro-name ,number-args ',processing-function))))

;;; define it as a function:
(defun argument (self pos)
  "Positional argument accessor."
  (when (slot-exists-p self 'arguments)
    (assert (<= pos (length (arguments  self ))) nil
            "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
            pos  self  (length (arguments self )))
    (elt  (arguments self)  (- pos 1 ))))

(defun name-of-child  (self pos)
  "Name of child by position."
  (when (slot-exists-p self 'arguments)
    (assert (<= pos (length (arguments  self ))) nil
            "In ~a:Not that many arguments:  n = ~a, found ~a arguments. "
            pos self  (length (arguments self )))
    (cond
      ((null  (children-are-called self )) nil)
      ((listp (children-are-called  self))
       (elt  (children-are-called self)  (- pos 1 )))
      ((atom  (children-are-called self ))
       (children-are-called self)))))
                                        
        
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
;;; Now define the processing function:
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
           (setf (anumber self )
                 (increment-counter-value  (class-name (class-of self )))))
                                        ; Now process the label,
                                        ;it will automatically point to the referend
         (if ,label-first
             (funcall processor (first arguments ))
             (funcall processor (second arguments )))
         (pop-enclosing-referend)
         self)
       )
;;; define precedence
     (when ',precedence
       (define-precedence ,macro-name :same-as ',precedence))
;;; Install processing function
     (define-tex-macro ,macro-name 2 ',processing-function)))

;;}}}



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
      (name-of-child this-parent (or child-position  1) ))
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
