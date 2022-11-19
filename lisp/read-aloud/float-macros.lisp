;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)
;;; Mon Oct 25 08:54:44 EDT 1993
 ;;; Macros used by the module for handling floating objects.


  ;;; Macro: WITHOUT-FLOAT                                     Author: raman
  ;;; Created: Mon Oct 25 08:55:10 1993

(defmacro without-float ((object) &body body) 
  "Execute body with floating reading rule  for object
and floating reading style deactivated. "
  `(let ((object-name (class-name (class-of ,object )))
         (rule-flag  (eql 'float
                          (active-rule ,object )))
         (style-flag  (find 'float (current-reading-style ))))
    (unwind-protect 
         (progn
           (when rule-flag (deactivate-rule object-name   ))
           (when style-flag (deactivate-style 'float))
           ,@body )
      (when rule-flag (activate-rule object-name 'float))
      (when style-flag (activate-style 'float ))))
  )


