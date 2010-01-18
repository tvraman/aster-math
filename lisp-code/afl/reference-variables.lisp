;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; implements reference variables. Uses a mutable object structure
;;; with one field val. A method reference-value is written to make
;;; the interface clean. If passed any lisp object reference-value
;;; returns it, except for objects of type reference in which case the
;;; slot value is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; { Structure: REFERENCE                                     Author: raman
;;; Created: Fri Aug  7 11:27:54 1992
;;; A mutable object that implements reference variables
(defstruct reference
  val
  )
;;; }

;;; { method reference-value

;;; Overloading reference-value. If argument is not a reference
;;; object, just return it, otherwise return the slot value.
;;; Method: REFERENCE-VALUE                                  Author: raman
;;; Created: Fri Aug  7 11:44:54 1992
(defmethod reference-value ((ordinary t) )
  "return argument"
  ordinary
  )

;;; Method: REFERENCE-VALUE                                  Author: raman
;;; Created: Fri Aug  7 11:48:37 1992
(defmethod reference-value ((reference reference))
  "return value of ref"
  (reference-val reference)
  )

;;; }


;;; This function is taken from Norvig's book.
;;; It is useful for building up function names etc.
;;; 
(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))
