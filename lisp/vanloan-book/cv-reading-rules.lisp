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
  (read-aloud " matrix, ")
  )
