;;;   -*-   Mode: LISP -*-    ;;;
 
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)


;;; Map reading states to afl states.

;;; Variable: *READING-STATE-TABLE*                          Author: raman
;;; Created: Thu Sep  3 14:28:04 1992

(defvar *reading-state-table*
  (make-hash-table :test #'eq  )
  "Association between reading states and afl states")

;;; Function: DEFINE-READING-STATE                           Author: raman
;;; Created: Thu Sep  3 14:29:55 1992

(defun define-reading-state (state-name  afl-setter) 
  "define afl-setter as the setter for this reading state"
  (setf (gethash state-name  *reading-state-table* ) afl-setter))

;;; Function: GET-READING-STATE                              Author: raman
;;; Created: Thu Sep  3 14:31:07 1992

(defun get-reading-state (state) 
  "Retrieve state setter for this state"
  (or (gethash state *reading-state-table*) #'identity))

;;; Macro: READING-STATE                                     Author: raman
;;; Created: Sat Oct 10 11:11:25 1992

(defun reading-state (state-name) 
  "Return a point in speech space reached by applying the handler for state state-name"
  (funcall (get-reading-state state-name) afl:*current-speech-state*))

;;; Macro: WITH-READING-STATE                                Author: raman
;;; Created: Fri Nov  6 09:46:45 1992

(defmacro with-reading-state (state &body body) 
  "Execute body in a block after setting state"
  `(afl:new-block (afl:local-set-state ,state)
     ,@body))
