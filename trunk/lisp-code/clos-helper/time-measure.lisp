;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)



  ;;; Function: TIME-INTERVAL                                  Author: raman
  ;;; Created: Fri Jan  7 17:03:29 1994

  ;;; Variable: *BASE-TIME*                                    Author: raman
  ;;; Created: Fri Jan  7 17:20:06 1994

(defparameter  *base-time*
  (remove nil
          (multiple-value-list (decode-universal-time 0))) "Base time")

(defun time-interval (u-time-1 u-time-2) 
  "Compute the time interval between u-time-1 and u-time-2. Arguments are
assumed to be in universal time as defined by lisp."
  (let
      ((gap (abs (- u-time-2 u-time-1 )))
       (time-gap nil)
       (result nil))
    (setf time-gap
          (remove nil (multiple-value-list
              (decode-universal-time gap ))))
    (setf result
          (nreverse (mapcar #'- time-gap  *base-time* )))
    
    )
  )
