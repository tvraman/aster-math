;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :cl-user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Mon Oct 25 09:10:45 EDT 1993
 ;;;
;;; Implements float-if and delay-until
;;; Structure delay-object has two slots:
;;; object-name is an object name from the document class hierarchy,
;;; in general, any symbol. 
;;; delayed-form is the delayed form 
;;; Function delay-until takes an object name and a function object.
;;; It creates  the corresponding delay-object and installs it.
;;; Function force-if is called with a symbol.
;;; It forces all delay-objects that have this symbol as their
;;; object-name
;;;


  ;;; Variable: *DELAYED-OBJECTS-LIST*                         Author: raman
  ;;; Created: Mon Oct 25 09:14:20 1993

(defvar *delayed-objects-list* nil "List of currently delayed objects. ")


  ;;; Structure: DELAY-OBJECT                                  Author: raman
  ;;; Created: Mon Oct 25 09:14:44 1993

;;;  A delay  object. Name is a symbol, and will trigger the force; Form
;;; is a function object that will be executed when this delay is forced. 

(defstruct delay-object
  name
  form)


  ;;; Function: DELAY-UNTIL                                    Author: raman
  ;;; Created: Mon Oct 25 09:16:31 1993

(defun delay-until (name form ) 
  "Delay evaluation of this form until triggered by name. "
  (setq *delayed-objects-list*
        (nreverse (cons (make-delay-object :name name
                                           :form form) *delayed-objects-list*)))
  )


  ;;; Function: FORCE-IF                                       Author: raman
  ;;; Created: Mon Oct 25 09:20:23 1993

(defun force-if (name) 
  "Force all delayed objects that are triggered by name. "
  (loop for delay in *delayed-objects-list*
        when (eql name (delay-object-name delay))
        do (funcall (delay-object-form delay ))
        (setf *delayed-objects-list* (remove delay *delayed-objects-list* ))
        )
  )





  ;;; Function: FORCE-ALL-FLOATS                               Author: raman
  ;;; Created: Tue Oct 26 09:06:41 1993

(defun force-all-floats () 
  "Forces all floats to be rendered. Use this to clean up. "
  (loop for delay in *delayed-objects-list*
        do (funcall (delay-object-form delay )))
  (reset-footnote-counter)
  (setf *delayed-objects-list* nil)
  )
