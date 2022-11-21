;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Sun Mar 28 14:35:44 EST 1993;;; Macros used by browser
 


  ;;; Macro: SAVE-POINTER-EXCURSION                            Author: raman
  ;;; Created: Sun Mar 28 14:36:01 1993

(defmacro save-pointer-excursion (&body body) 
  "Save excursion"
  `(let
    ((save-pointer *read-pointer* ))
    (unwind-protect
         (progn
           ,@body)
      (setf *read-pointer* save-pointer))
    (values))
  )

