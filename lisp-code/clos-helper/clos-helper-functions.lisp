;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(use-package :clos)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Sun Feb 14 09:37:53 EST 1993
;;; <(clos helper functions taken from lisp faq)>

(defmethod class-slot-names ((instance standard-object))
  "Given an INSTANCE, returns a list of the slots in the instance's class."
  (mapcar #'slot-definition-name
          (class-slots (class-of instance))))

(defun clos-class-slot-names (class-name)
  "Given a CLASS-NAME, returns a list of the slots in the class."
  (mapcar #'slot-definition-name
          (class-slots (find-class class-name))))
