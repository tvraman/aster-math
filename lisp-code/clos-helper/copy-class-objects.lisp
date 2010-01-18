;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)
(use-package :clos)

;;; Sun Apr  4 19:45:50 EDT 1993
;;; To copy class objects one level.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;; Method: COPY-OBJECT                       Author: raman
  ;;; Created: Sun Apr  4 19:46:26 1993

(defmethod copy-object ((object standard-object))
  "Copy standard clos objects. "
  (let ((clone (make-instance (class-name (class-of object ))))
        (slot-names(mapcar #':slot-definition-name
                           (class-slots (class-of object )))))
    (loop for slot-name  in slot-names do
          (setf (slot-value clone slot-name )
                (slot-value object slot-name)))
    clone)
  )


(defmethod copy-object ((object t))
  "Default method return nil"
  nil)
