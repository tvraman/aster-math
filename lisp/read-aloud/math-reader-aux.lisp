 ;;;   -*-   Mode: LISP -*-    ;;;
 ;;;                                                                       ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :cl-user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


 ;;; Contains all the helper functions and methods used by the simple
 ;;; reading rule for math objects.
 ;;; Tue Dec  8 13:53:34 EST 1992
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;;; Tue Nov 24 10:54:42 EST 1992
 ;;; Contains some reading rules for reading math expressions.
 ;;; These rules are being written using a very primitive
 ;;; representation.
 ;;; Develop the right intermediate representation based on these
 ;;; rules.
 ;;; First consider reading math expressions with no user defined 
 ;;; objects. 

 ;;; A math object is a leaf if it has no children.


 ;;; Method: LEAF-P                                           Author: raman
 ;;; Created: Tue Nov 24 11:34:18 1992

(defmethod leaf-p ((math-object math-object))
  "Is this  a leaf?"
  (null (children math-object))
  )

(defmethod leaf-p ((math-array math-array)) nil)


 ;;; Method: LEAF-P                                           Author: raman
 ;;; Created: Sun Nov 29 16:19:22 1992

(defmethod leaf-p ((string string ))
  "Simple strings are leaves"
  t)
(defmethod leaf-p ((math-subformula math-subformula))
  "Is this subformula a leaf?"
  (leaf-p (contents math-subformula))
  )

(defmethod leaf-p ((integral-d integral-d))
  "Integral-d is a leaf. "
  t)
(defmethod leaf-p (( list list))
  "Are these all leaves? "
  (every #'leaf-p list)
  )

(defmethod leaf-p ((factorial factorial ))
  "Is a leaf it's contents is a leaf"
  (leaf-p (contents factorial ))
  )
 ;;; Variable: *INF-READ-THRESHOLD*                           Author: raman
 ;;; Created: Tue Nov 24 15:49:48 1992

(defvar *inf-read-threshold* 3
  "If more than this many children read as prefix")

 ;;; Function: READ-AS-INFIX                                  Author: raman
 ;;; Created: Tue Nov 24 15:41:34 1992

(defvar *use-comma-intonation-for-lists* nil
  "if t use comma intonation when reading long lists")

(defmethod  read-as-infix ((math-object math-object ))
  "Read as infix"
  (flet
   ((all-have-same-weight (list)
                          (let ((first-weight (weight (first list  ))))
                            (loop for item in (rest list)
                                  always (= first-weight
                                            (weight item ))))))
   (let*  ((children (children math-object ))
           (comma-flag(and *use-comma-intonation-for-lists*
                           (> (length children ) 2)
                           (all-have-same-weight children ))))
     (read-math-child   (first children))
     (afl:tts-queue " ")
     (unless (rest children )
       (read-math-object-and-attributes math-object))
     (loop for child in (rest children) 
           do
           (when comma-flag (afl:tts-speak "[_,]"))
           (read-math-object-and-attributes math-object)
           (read-math-child  child ))))
  )


 ;;; Parameter: *PAUSE-AROUND-CHILD*                            Author: raman
 ;;; Created: Thu Nov 26 15:52:33 1992

(defparameter *pause-around-child*  5
  "Pause  surrounding reading of child")

 ;;; Modified: Sun Dec  6 09:47:52 EST 1992
 ;;; read aloud rule for math object puts pause if necessary, the rest
 ;;; of the helping functions need not do this any more. 
 ;;; Function: READ-MATH-CHILD                                Author: raman
 ;;; Created: Tue Nov 24 19:11:00 1992
;;; Modified: Mon Dec 28 10:07:05 EST 1992
;;; objects now have a parent link, so read-math-child needs only be
;;; passed the child. In fact read-math-child  is almost obselete.
;;; Also removed *be-smart-about-division* before backing up. 
;;; <(backed up old version. )>

(defmethod read-math-child ((ordinary t ))
  (read-aloud ordinary )
  )



(defmethod  read-math-child ((child math-object ))
  "Read math child "
  (let ((parent (parent child )))
    (cond
      ((or (null parent )
           (equal 'undefined parent )) (read-aloud child ))
      ( (leaf-p child) (read-aloud child ))
      ((math-subformula-p  child) (read-aloud child))
      ((delimited-expression-p child) (read-aloud child))
      ((operand? child) (read-aloud child))
      ((precedence->  parent child :key #'contents)
       (with-reading-state (reading-state 'children)
         (read-aloud child )))
      (t (read-aloud  child ))
      )
    )
  )


 ;;; Method: READ-AS-PREFIX                                   Author: raman
 ;;; Created: Sun Dec  6 11:01:55 1992

(defmethod read-as-prefix ((big-operator big-operator))
  "Read  large operator object"
  (read-math-object-and-attributes big-operator)
  (with-reading-state (reading-state 'children)
    (loop for child in (children big-operator )
          do (read-aloud child )))
  )

 ;;; Method: READ-AS-PREFIX                                 Author: raman
 ;;; Created: Tue Nov 24 15:45:36 1992

(defmethod  read-as-prefix ((math-object math-object) )
  "Read as prefix"
  (read-math-object-and-attributes math-object)
  (cond
    ((= 1 (weight  (children math-object )))
     (read-aloud (children math-object )))
    (t (with-reading-state (reading-state 'children)
         (loop for child in (children math-object )
               do (read-aloud child )))))
  )

 ;;; Function: READ-MATH-OBJECT-AND-ATTRIBUTES                Author: raman
 ;;; Created: Tue Nov 24 14:36:26 1992
(defun read-math-object-and-attributes (math-object) 
  "Read the object and its attributes"
  (read-aloud (contents math-object ))
  (afl:tts-queue " ")
  (when (attributes math-object)
    (mapc #'read-aloud 
          (sorted-attributes  (attributes math-object ))))
  )


 ;;; Variable: *OBJECTS-READ-AS-INFIX*                        Author: raman
 ;;; Created: Tue Nov 24 13:49:44 1992

(defvar *objects-read-as-infix*
  '(arrow-operator relational-operator binary-operator)
  "These are read as infix")

 ;;; Function: READ-AS-INFIX?                                 Author: raman
 ;;; Created: Tue Nov 24 13:07:01 1992

(defun read-as-infix? (math-object) 
  "Is this read as infix?"
  (some  #'(lambda(type) (typep math-object type)) *objects-read-as-infix*)
  )




 ;;; Variable: *OBJECTS-READ-AS-PREFIX*                       Author: raman
 ;;; Created: Tue Nov 24 13:51:29 1992

(defparameter  *objects-read-as-prefix*
  '(big-operator negation-operator unary-minus
    quantifier mathematical-function-name delimited-expression
    square-root
    )
  "These are read as prefix")
;;; Modified: Mon Dec 28 11:10:31 EST 1992
;;; converting to method. 
 ;;; Method: READ-AS-PREFIX?                                Author: raman
 ;;; Created: Tue Nov 24 13:15:01 1992

(defmethod  read-as-prefix? ((math-object  math-object ))
  "Is this read as prefix?"
  (or 
   (some  #'(lambda(type) (typep math-object type)) *objects-read-as-prefix* )
                                        ;disjunct2
   (and   (binary-operator-p math-object)
          (> (length (children math-object))
             *inf-read-threshold* ))    ;disjunct3
   )
  )

(defmethod read-as-prefix? ((math-subformula math-subformula ))
  (read-as-prefix? (contents math-subformula ))
  )





;;; Method: COMPUTE-PAUSE                                          Author: raman
;;; Created: Thu Dec 10 10:32:18 1992

(defmethod compute-pause ((math-object math-object))
  "Compute amount of pause to put around this object"
  (declare (fixnum *pause-around-child* ))
  (let* 
      ((object-weight(the fixnum 
                          (weight math-object )))
       (pause-duration (the fixnum
                            (if (> object-weight 1)
                                (the fixnum (* object-weight *pause-around-child*) )
                                0))))
    pause-duration)
  )



;;; Method: READ-ATTRIBUTES                                  Author: raman
;;; Created: Fri Dec 11 12:58:52 1992

(defmethod read-attributes ((math-object math-object))
  "Read out all the attributes"
  (when (attributes math-object)
    (mapc #'read-aloud
          (sorted-attributes (attributes math-object ))))
  )
