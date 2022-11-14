;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Sun Dec 20 13:21:39 EST 1992
;;; Reading rule definitions for objects defined in VanLoan's book.
;;; Object definitions are in <(file object definitions)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-reading-rule (c-vector simple)
  "Simple reading rule for a  vector"
  (read-aloud   (argument c-vector 1))
  (read-aloud "denote a complex " )
  (read-aloud  (argument c-vector 2 ))
  (read-aloud " vector, ")
  )

(def-reading-rule (c-matrix simple)
  "Simple reading rule for a  complex matrix"
  (read-aloud   (argument c-matrix 1))
  (read-aloud "denote a complex " )
  (read-aloud  (argument c-matrix 2 ))
  (read-aloud " by ")
  (read-aloud (argument c-matrix 3))
  (read-aloud " matrix, "))

(defmethod read-aloud  (( c-vector c-vector ))
  "Read aloud method for object c-vector "
  (afl:with-pronunciation-mode  (:mode :math)
    (read-aloud  "the ")
    (read-aloud (argument c-vector 2) )
    (read-aloud "vector ")
    (read-aloud (argument c-vector 1 ))))

(defmethod read-aloud  (( c-matrix c-matrix ))
  "Read aloud method for object c-matrix "
  (afl:with-pronunciation-mode (:mode :math)
    (read-aloud "matrix, ")
    (read-aloud (argument c-matrix 1))
    (read-aloud "is" )
    (read-aloud (argument c-matrix  2))
    (read-aloud "by ")
    (read-aloud (argument c-matrix 3))))

(defmethod read-aloud  (( quad quad ))
  "Read aloud method for object quad "
  (afl:tts-pause 10))

(defmethod read-aloud  (( qquad qquad ))
  "Read aloud method for object qquad "
  (afl:tts-pause 10))

(defmethod read-aloud  (( two-by-two-matrix two-by-two-matrix ))
  "Read aloud method for object two-by-two-matrix "
  (let
      ((array (make-instance
               'math-array
               :contents
               (list
                (list (argument two-by-two-matrix 1)
                      (argument two-by-two-matrix 2))
                (list (argument two-by-two-matrix 3)
                      (argument two-by-two-matrix 4))))))
    (read-aloud array)))

(defmethod read-aloud  (( two-vector two-vector ))
  "Read aloud method for object two-vector "
  (let
      ((vector (make-instance 'math-array
                              :contents (list
                                         (arguments two-vector )))))
    (read-aloud vector)))

(defmethod read-aloud  (( r-matrix r-matrix ))
  "Read aloud method for object r-matrix "
  (afl:with-pronunciation-mode (:mode :math)
    (read-aloud "matrix, ")
    (read-aloud (argument r-matrix 1))
    (read-aloud "is" )
    (read-aloud (argument r-matrix  2))
    (read-aloud "by ")
    (read-aloud (argument r-matrix 3))))

(def-reading-rule (kronecker prefix)
  "Simple reading rule for kronecker product. "
  (let ((children (children kronecker )))
    (read-aloud  "kronecker product of")
    (afl:subclause-boundary)
    (read-aloud  (first children ))
    (read-aloud "and" )
    (read-aloud (second children ))))


(def-reading-rule (kronecker-product prefix)
  "Simple reading rule for kronecker product. "
  (let ((children (children kronecker-product )))
    (read-aloud  "kronecker product of,  ")
    (read-aloud  (first children ))
    (read-aloud "and" )
    (read-aloud (second children ))))
