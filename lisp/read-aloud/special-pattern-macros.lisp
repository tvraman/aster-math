;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)


;;; Sun Dec 27 11:17:31 EST 1992
;;;  Contains macro definition def-special-pattern and associated
;;;  code:
 
;;; Scanning for special patterns is buggy since if  the pattern
;;; depends on some slot of the object that gets assigned later, the
;;; instance will be wrongly marked.  the slot special-pattern should
;;; be removed, and instead define a generic method that recognizes
;;; the pattern just before reading the object.
;;; Chances are that this will slow the reader down too much.
;;;
;;; <(Backing up old version. )>
 


  ;;; Macro: DEF-SPECIAL-PATTERN                               Author: raman
  ;;; Created: Sun Dec 27 11:19:35 1992

(defmacro def-special-pattern (object-name &rest rules) 
  "Define special patterns for this object"
  `(defmethod   special-pattern  ((instance  ,object-name ))
    "special patterns defined"
    (declare (ignore initargs ))
    (cond
      .,(loop  for rule in rules
               collect
               `(( funcall  ,(first rule) instance)
                 ',(second rule)
                 )
               )
      )
    )
  )
