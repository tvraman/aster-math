;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
;;; Thu Apr  8 09:03:31 EDT 1993

;;; The children and attributes of specific math objects have specific
;;; names.
;;; E.G. The subscript of an integral is the lower limit, its child is
;;; the integrand.
;;; Such information is useful in generating messages that locate
;;; current position in the browser.
;;; This could be implemented by having slots children-are-called and
;;; attributes-are-called in the math-object.
;;; Alternately we can just implement these as methods on those
;;; objects where we have the information.
;;; To postpone the decision, use the macro
;;; call-its-children and call-its-attributes which will be used the
;;; user to provide the information.
;;; For the present these macros generate method definitions, if later
;;; it is decided to keep this information in a slot in the
;;; math-object, all that will have to be changed is the macro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;; Macro: CALL-ITS-CHILDREN                                 Author: raman
  ;;; Created: Thu Apr  8 09:05:26 1993

(defmacro call-its-children (object name-spec) 
  "Children of object are called as specified by name-spec.
Note: call here used as in a spade is called a spade."
  `(progn
    (defmethod children-are-called ((self ,object))
      ,(format nil "Automatically generated method for class ~a" object)
      ',name-spec)
    (defmethod name-of-child  ((,object ,object) (n integer))
      "Automatically generated name of child  accessor"
      (let
          ((children-called (children-are-called  ,object )))
        (cond
          ((listp children-called)
           (if (<= n (length children-called ))
               (elt  children-called   (- n 1 ))
               (last children-called )))
          ((atom  children-called )
           (format nil "~a ~a "
                   (cardinal-number n) children-called ))
          (t (error "Should not have got here. "))
          )
        )
      )
                                        ; with calling sequence reversed
    (defmethod name-of-child  ((n integer) (,object ,object))
      "Automatically generated name of child  accessor"
      (let
          ((children-called (children-are-called  ,object )))
        (cond
          ((listp children-called)
           (if (<=  n (length children-called ))
               (elt  children-called   (- n 1 ))
               (last children-called )))
          ((atom  children-called )
           (format nil "~a ~a "
                   (cardinal-number n) children-called ))
          (t (error "Should not have got here. "))
          )
        )
      ))
  )




(defmacro call-its-attributes (object name-spec) 
  "Attributes of object are called as specified by name-spec.
Note: call here used as in a spade is called a spade."
  `(defmethod attributes-are-called ((self ,object))
    ,(format nil "Automatically generated method for class ~a" object)
    ',name-spec)
  )

