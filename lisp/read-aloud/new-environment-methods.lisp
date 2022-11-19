;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :aster)
;;; Separating methods on new-environments into their own file:

(defun caption-p (self)
  (typep  self 'caption))



(defmethod caption ((figure figure ))
  "Extract the caption from a figure if any. "
  (find-if
   #'caption-p
   (contents figure )))

(defmethod caption ((table table))
  "Extract the caption from a table  if any. "
  (find-if
   #'caption-p
   (contents table )))







