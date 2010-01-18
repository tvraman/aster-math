;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)


;;; Sun Dec 20 13:20:09 EST 1992
;;; Contains object definitions for macros occurring in VanLoan's
;;; book. 
;;; Will also contain the default reading rule.
;;; Load this file before any other reading rule definitions for these
;;; objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; { van loan's book on fourier transforms

;;; { Kroniker product
(define-text-object     :macro-name "kronecker"
  :number-args 0
  :processing-function kronecker-expand 
  :precedence  multiplication
  :object-name kronecker
  :supers (binary-operator)
  )


(define-text-object     :macro-name "T"
  :number-args 0
  :processing-function t-expand 
  :precedence  multiplication
  :object-name kronecker-product
  :supers (binary-operator)
  )

;;; Object has 0 slots

(def-reading-rule (kronecker prefix)
    "Simple reading rule for kronecker product. "
  (let ((children (children kronecker ))) 
    (read-aloud  "kronecker product of")
    (afl:subclause-boundary)
    (read-aloud  (first children ))
    (read-aloud "and" )
    (read-aloud (second children ))
    )
  )
(def-reading-rule (kronecker-product prefix)
"Simple reading rule for kronecker product. "
  (let ((children (children kronecker-product ))) 
    (read-aloud  "kronecker product of,  ")
    (read-aloud  (first children ))
    (read-aloud "and" )
    (read-aloud (second children ))
    )
  )


;;; }
;;; { Vectors and matrices

(define-text-object     :macro-name "Cinv" 
  :number-args 2
  :processing-function c-vector-expand 
  :precedence  nil 
  :object-name c-vector
  :supers (math-object)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( c-vector c-vector )) 
  "Read aloud method for object c-vector "
  (afl:with-pronunciation-mode  (:mode :math) 
    (read-aloud  "the ")
    (read-aloud (argument c-vector 2) )
    (read-aloud "vector ")
    (read-aloud (argument c-vector 1 )))
  )

;(activate-rule 'c-vector 'default)

(define-text-object     :macro-name "Cinm" 
  :number-args 3
  :processing-function c-matrix-expand 
  :precedence  nil 
  :object-name c-matrix
  :supers (math-object)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 3)  in                         read-aloud 
(defmethod read-aloud  (( c-matrix c-matrix )) 
  "Read aloud method for object c-matrix "
  (afl:with-pronunciation-mode (:mode :math) 
    (read-aloud "matrix, ")
    (read-aloud (argument c-matrix 1))
    (read-aloud "is" ) 
    (read-aloud (argument c-matrix  2))
    (read-aloud "by ")
    (read-aloud (argument c-matrix 3)))
  )

;(activate-rule 'c-matrix 'default)

;;; }
;;; {Misc

(define-text-object     :macro-name "quad" 
  :number-args 0
  :processing-function quad-expand 
  :precedence  nil 
  :object-name quad
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( quad quad )) 
  "Read aloud method for object quad "
(afl:pause 10)
  )

(define-text-object     :macro-name "qquad" 
  :number-args 0
  :processing-function qquad-expand 
  :precedence  nil 
  :object-name qquad
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( qquad qquad )) 
  "Read aloud method for object qquad "
(afl:pause 10)
  )



;;; }
;;; {matrix computations book:
(define-text-object     :macro-name "twon" 
  :number-args 4
  :processing-function twon-expand 
  :precedence  nil 
  :object-name two-by-two-matrix
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 4)  in                         read-aloud 
(defmethod read-aloud  (( two-by-two-matrix two-by-two-matrix )) 
  "Read aloud method for object two-by-two-matrix "
  (let
      ((array (make-instance 'math-array
                             :contents (list
                                        (list (argument
                                               two-by-two-matrix 1) 
                                              (argument two-by-two-matrix 2))
                                        (list (argument two-by-two-matrix 3)
                                              (argument two-by-two-matrix 4))))))
    (read-aloud array)
    )
  )




(define-text-object     :macro-name "twovn" 
  :number-args 2
  :processing-function twovn-expand 
  :precedence  nil 
  :object-name two-vector
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( two-vector two-vector )) 
  "Read aloud method for object two-vector "
  (let
      ((vector (make-instance 'math-array
                              :contents (list
                                         (arguments two-vector )))))
    (read-aloud vector))
  )

(define-text-object     :macro-name "inm" 
  :number-args 3
  :processing-function inm-expand 
  :precedence  nil 
  :object-name r-matrix
  :supers (math-object)
  )

(defmethod read-aloud  (( r-matrix r-matrix )) 
  "Read aloud method for object r-matrix "
  (afl:with-pronunciation-mode (:mode :math) 
    (read-aloud "matrix, ")
    (read-aloud (argument r-matrix 1))
    (read-aloud "is" ) 
    (read-aloud (argument r-matrix  2))
    (read-aloud "by ")
    (read-aloud (argument r-matrix 3)))
  )
(activate-rule 'r-matrix 'default)

;;; }

;;; }

 
