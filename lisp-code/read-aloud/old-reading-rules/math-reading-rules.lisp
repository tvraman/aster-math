;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
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
  (null (math-object-children math-object))
  )


;;; Method: LEAF-P                                           Author: raman
;;; Created: Sun Nov 29 16:19:22 1992

(defmethod leaf-p ((math-subformula math-subformula))
  "Is this subformula a leaf?"
  (leaf-p (math-object-contents math-subformula))
  )

(defmethod leaf-p (( list list))
  "Are these all leaves? "
  (every #'leaf-p list)
  )

(define-reading-rule  (:rule-name simple :class math-object)
    (let* 
        ((object-weight (weight math-object ))
         (pause-duration (if (> object-weight 1)
                             (* object-weight *pause-around-child*) 
                             0)))
      (afl:with-surrounding-pause pause-duration 
        (cond
          ((leaf-p math-object) (read-math-object-and-attributes math-object))
          ( (read-as-infix?  math-object) (read-as-infix math-object))
          ((read-as-prefix?  math-object) (read-as-prefix math-object))
          (t (error "Do not know what to do ~%"))
          )))
  )


;;; Variable: *INF-READ-THRESHOLD*                           Author: raman
;;; Created: Tue Nov 24 15:49:48 1992

(defvar *inf-read-threshold* 3
  "If more than this many children read as prefix")

;;; Function: READ-AS-INFIX                                  Author: raman
;;; Created: Tue Nov 24 15:41:34 1992

(defmethod  read-as-infix ((math-object math-object ))
  "Read as infix"
  (let ((children (math-object-children math-object )))
       (read-math-child math-object  (first children))
       (dectalk:space)
       (loop for child in (rest children) 
             do 
             (read-math-object-and-attributes math-object)    
             (read-math-child math-object child )))
  )




;;; Variable: *BE-SMART-ABOUT-DIVISION*                      Author: raman
;;; Created: Thu Nov 26 11:34:37 1992

(defvar *be-smart-about-division* t
  "If t then cue division explicitly regardless of precedence of
parent.")




;;; Parameter: *PAUSE-AROUND-CHILD*                            Author: raman
;;; Created: Thu Nov 26 15:52:33 1992

(defparameter *pause-around-child*  25
  "Pause  surrounding reading of child")
;;; Modified: Sun Dec  6 09:47:52 EST 1992
;;; read aloud rule for math object puts pause if necessary, the rest
;;; of the helping functions need not do this any more. 
;;; Function: READ-MATH-CHILD                                Author: raman
;;; Created: Tue Nov 24 19:11:00 1992

(defun read-math-child (parent child) 
  "Read math child "
  (cond
    ( (leaf-p child)
     (read-aloud child ))
    ((math-subformula-p  child) (read-aloud child))
    ((delimited-expression-p child) (read-aloud child))
    ((precedence->  parent child :key #'math-object-contents)
     (with-reading-state (reading-state 'children)
       (read-aloud child )))
    ((and *be-smart-about-division* 
          (equal "/" (math-object-contents child)))
;     (with-reading-state (reading-state 'children)
;       (read-aloud child ))
     (read-aloud "fraction, ")
     (read-aloud child))
    (t (read-aloud  child ))
    )
  )


;;; Method: READ-AS-PREFIX                                   Author: raman
;;; Created: Sun Dec  6 11:01:55 1992

(defmethod read-as-prefix ((big-operator big-operator))
  "Read  large operator object"
  (read-math-object-and-attributes big-operator)
  (with-reading-state (reading-state 'children)
    (loop for child in (math-object-children big-operator )
          do (read-aloud child )))
  )

;;; Method: READ-AS-PREFIX                                 Author: raman
;;; Created: Tue Nov 24 15:45:36 1992

(defmethod  read-as-prefix ((math-object math-object) )
  "Read as prefix"
  (read-math-object-and-attributes math-object)
  (cond
    ((= 1 (weight  (math-object-children math-object )))
     (read-aloud (math-object-children math-object )))
    (t (with-reading-state (reading-state 'children)
         (loop for child in (math-object-children math-object )
               do (read-aloud child )))))
  )

;;; Function: READ-MATH-OBJECT-AND-ATTRIBUTES                Author: raman
;;; Created: Tue Nov 24 14:36:26 1992
(proclaim '(inline read-math-object-and-attributes))
(defun read-math-object-and-attributes (math-object) 
  "Read the object and its attributes"
  (read-aloud (math-object-contents math-object ))
  (dectalk:space)
  (when (math-object-attributes math-object)
    (mapc #'read-aloud 
            (sorted-attributes  (math-object-attributes math-object ))))
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
  (or
   (find  (class-name (class-of math-object)) *objects-read-as-infix*)
   (find (math-object-type math-object) *objects-read-as-infix*))
  )



;;; Variable: *OBJECTS-READ-AS-PREFIX*                       Author: raman
;;; Created: Tue Nov 24 13:51:29 1992

(defparameter  *objects-read-as-prefix*
  '(big-operator negation-operator unary-minus
    quantifier mathematical-function-name delimited-expression )
  "These are read as prefix")
;;; Function: READ-AS-PREFIX?                                Author: raman
;;; Created: Tue Nov 24 13:15:01 1992

(defun read-as-prefix? (math-object) 
  "Is this read as prefix?"
  (or
   (find  (class-name (class-of math-object))
          *objects-read-as-prefix*)     ;disjunct1
   (find (math-object-type math-object) *objects-read-as-prefix*) ;disjunct2
   (and   (binary-operator-p math-object)
          (> (length children)  *inf-read-threshold* )) ;disjunct3
   )
  )



(define-reading-rule (:rule-name simple :class fraction)
    (cond
      ((and (leaf-p (fraction-numerator fraction))
            (leaf-p (fraction-denominator fraction )))
       (read-aloud (fraction-numerator fraction))
       (read-aloud "over" ) 
       (read-aloud
        (fraction-denominator fraction )))
      (t (read-aloud " fraction, ")
         (with-reading-state (reading-state 'children)
           (read-aloud (fraction-numerator fraction )))
         (read-aloud  " divided by,  ")
         (with-reading-state (reading-state 'children)
           (read-aloud (fraction-denominator fraction )))
         ))
  )



(define-reading-rule (:rule-name simple :class delimited-expression)
    (afl:with-surrounding-pause (* *pause-around-child*
                                   (weight  delimited-expression))
      (read-aloud(first  (math-object-contents
                          delimited-expression )))
      (with-reading-state (reading-state 'children)
        (loop for child in (math-object-children delimited-expression)
              do           (read-aloud child  )))
      (when (math-object-attributes delimited-expression)
        (read-aloud (close-delimiter delimited-expression))
        (mapc #'read-aloud 
                (sorted-attributes  (math-object-attributes
                                     delimited-expression )))))
  )


(define-reading-rule (:rule-name simple :class square-root) 
    (read-aloud " square root ")
  (cond
    ((and (math-object-subtype-p (argument-1 square-root))
          (leaf-p (argument-1 square-root ))
          (null (math-object-attributes  (argument-1 square-root ))))
     (read-aloud (argument-1 square-root )))
    (t(read-aloud " of ")
      (with-reading-state (reading-state 'children) 
        (read-aloud (argument-1 square-root ))))
      )
    )

