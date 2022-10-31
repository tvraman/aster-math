;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Teaching myself how to use around methods.

(defmethod read-aloud  :around ((document document))
           "Around method"
           
           (if (and  *simple*
                     (compute-applicable-methods #'reading-rule
                                                 (list document
                                                       'simple )))
               (reading-rule document 'simple) 
               (call-next-method))
           )


;;; Method: READING-RULE                                     Author: raman
;;; Created: Mon Dec  7 18:38:01 1992

(defmethod reading-rule ((math-object math-object) (rule-name (eql
                                                               'simple ))) 
  "Reading rule"
  (format dectalk:*stream* "Simplest rule says nothing! ") 
  )
