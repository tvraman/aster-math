;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)

;;; Mon Dec 21 10:21:44 EST 1992
;;; Contains reading rule definitions for objects from Vavasis' book

(defmethod read-aloud  (( xis xis ))
  "Read aloud method for object xis "
  (read-aloud " x eye star" ))

(defmethod read-aloud  (( xio xio ))
  "Read aloud method for object xio "
  (read-aloud "x eye hat"))

(defmethod read-aloud  (( nph nph ))
  "Read aloud method for object nph "
  (read-aloud " np hard "))
(defmethod read-aloud  (( ze ze ))
  "Read aloud method for object ze "
  (read-aloud "zero"))

(defmethod read-aloud  (( xs xs ))
  "Read aloud method for object xs "
  (read-aloud "x star"))
(defmethod read-aloud  (( ddfdxi ddfdxi ))
  "Read aloud method for object ddfdxi "
  (read-aloud  "partial derivative of f with respect to x i"))

(defmethod read-aloud  (( dfdxi dfdxi ))
  "Read aloud method for object dfdxi "
  (read-aloud "partial derivative of f with respect to x i"))

(defmethod read-aloud  (( bmu bmu ))
  "Read aloud method for object bmu "
  (setf (contents bmu) "bold mu" )
  (call-next-method))

(defmethod read-aloud  (( b-ref b-ref ))
  "Read aloud method for object b-ref "
  (read-aloud "refer to the book, ")
  (read-aloud (argument b-ref 1 ))
  (afl:comma-intonation)
  (afl:tts-force))
