;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Thu Dec  3 19:27:31 EST 1992
;;; Contains code to recognize complex objects.
;;; Complex objects in math:
;;; Shape of an object
;;; Weight of an object
;;; Deciding complexity:
;;; Deciding when to use variable substitution:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








  ;;; Method: WEIGHT                                           Author: raman
  ;;; Created: Sat Dec 12 11:22:50 1992

(defmethod weight  :around  ((document document))
           "Around method for memoizing weight"
           (cond
             ((internal-weight document) (internal-weight document))
             (t (setf (internal-weight  document)
                      (call-next-method))
                )
             )
           )

;;; Method: WEIGHT                                           Author: raman
;;; Created: Thu Dec 10 12:13:59 1992

(defmethod weight ((object t) )
  "Default weight method. "
  (if object 1 0)
  )
;;; Method: WEIGHT                                           Author: raman
;;; Created: Sat Dec  5 13:10:28 1992

(defmethod weight ((list list))
  "Weight of list of math objects"
  (loop for object in list
        sum (weight object))
  )


  ;;; Method: WEIGHT                                           Author: raman
  ;;; Created: Mon Sep 27 20:34:35 1993

(defmethod weight ((math-array math-array))
  "Weight of an array."
  (let ((contents (contents math-array ))) 
    (loop for row in  contents
          sum
          (loop for element in row sum (weight element ))))
  )

;;; Method: WEIGHT                                           Author: raman
;;; Created: Thu Dec  3 19:28:55 1992
;;; Modified: Sat Dec 12 13:17:20 EST 1992
;;; Removing unnecessary conditional clauses and replacing them by
;;; separate methods that specialize on their argument. 
;;; <(old method has been backed up. )>

(defmethod weight ((math-object math-object))
  "Return weight of this math object"
  (cond
    ((leaf-p math-object)
     (+ (weight (contents math-object ))
        (weight-of-attributes math-object )))
    (t (+ 1
          (weight-of-attributes  math-object) 
          (weight-of-children math-object )))
    )
  )




  ;;; Method: WEIGHT                                           Author: raman
  ;;; Created: Sat Dec 12 13:13:11 1992
;;; Modified: Thu Apr  8 11:06:32 EDT 1993
;;; <(Removed unnecessary cond, backed up. )>
(defmethod weight ((math-subformula math-subformula))
  "Weight of a subformula"
  (+  (weight  (contents math-subformula ))
      (weight-of-attributes math-subformula ))
  )


  ;;; Method: WEIGHT                                           Author: raman
  ;;; Created: Sat Dec 12 13:19:35 1992

(defmethod weight ((delimited-expression delimited-expression))
  "Weight of a delimited expression. "
  (+  1
      (weight-of-attributes delimited-expression)
      (weight-of-children delimited-expression ))
  )

;;; Method: WEIGHT-OF-ATTRIBUTES                             Author: raman
;;; Created: Thu Dec  3 19:45:07 1992

(defmethod weight-of-attributes ((math-object math-object))
  "Sum weights of all the attributes. "
  (or 
   (loop for attribute in (attributes math-object)
         sum (weight (attribute-value attribute )))
   0)
  )


;;; Method: WEIGHT-OF-CHILDREN                               Author: raman
;;; Created: Thu Dec  3 20:03:25 1992

(defmethod weight-of-children ((math-object math-object))
  "Sum weight of children"
  (or 
   (loop for child in (children math-object)
         sum (weight child))
   0)
  )

(defmethod  weight ((factorial factorial ))
  "weight of a factorial"
  (+ 1 (weight (contents factorial )))
  )

;;; Method: WEIGHT                                           Author: raman
;;; Created: Thu Dec 10 10:37:04 1992

(defmethod weight ((fraction fraction))
  "Weight of a fraction"
  (+ (weight (numerator-of fraction))
     (weight (denominator-of fraction )))
  )

(defmethod weight ((attribute attribute))
  "Weight of an attribute"
  (weight  (contents  attribute ))
  )
;;; Weight of a document:
;;; Thu Nov 18 09:27:01 EST 1993
 ;;; Weight of a document object can be used to guess how long it will
 ;;; take to render it.
;;; The formula is simple:
;;; The same recursive formulation as for math object.
;;; Note that now this will be the default weight method that will get
;;; called.
(defmethod weight((document document))
  "Weight of a document object."
(+ (weight (children document ))
   (weight (contents document )))
)

(defmethod weight ((article article ))
  "Weight of an article is weight of abstract plus rest. "
  (+ (weight (abstract article ))
     (call-next-method ))
  )
(defmethod weight ((object (eql 'undefined ))) 0)



(defmethod weight ((string string )) 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thu Dec 31 10:00:01 EST 1992
;;; Balanced math trees.


  ;;; Method: BALANCED-TREE-P                                  Author: raman
  ;;; Created: Thu Dec 31 10:00:20 1992

(defmethod balanced-tree-p ((math-object math-object))
  "Check if tree rooted here is balanced. "
  (let  ((children (children math-object )))
    (when children 
      (loop for current in (rest children )
            always (=
                    (weight current)
                    (weight (previous current  )))
            ))
    )
  )


