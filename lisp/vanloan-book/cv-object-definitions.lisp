;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Sun Dec 20 13:20:09 EST 1992
;;; Contains object definitions for macros occurring in VanLoan's
;;; book.
;;; Will also contain the default reading rule.
;;; Load this file before any other reading rule definitions for these
;;; objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ van loan's book on fourier transforms

;;{{{ Kroniker product
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

;;}}}
;;{{{ Vectors and matrices

(define-text-object     :macro-name "Cinv"
  :number-args 2
  :processing-function c-vector-expand
  :precedence  nil
  :object-name c-vector
  :supers (math-object)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud

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

                                        ;(activate-rule 'c-matrix 'default)

;;}}}
;;{{{Misc

(define-text-object     :macro-name "quad"
  :number-args 0
  :processing-function quad-expand
  :precedence  nil
  :object-name quad
  :supers (document)
  )

;;; Object has 0 slots

(define-text-object     :macro-name "qquad"
  :number-args 0
  :processing-function qquad-expand
  :precedence  nil
  :object-name qquad
  :supers (document)
  )

;;; Object has 0 slots

;;}}}
;;{{{matrix computations book:
(define-text-object     :macro-name "twon"
  :number-args 4
  :processing-function twon-expand
  :precedence  nil
  :object-name two-by-two-matrix
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 4)  in                         read-aloud

(define-text-object     :macro-name "twovn"
  :number-args 2
  :processing-function twovn-expand
  :precedence  nil
  :object-name two-vector
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud

(define-text-object     :macro-name "inm"
  :number-args 3
  :processing-function inm-expand
  :precedence  nil
  :object-name r-matrix
  :supers (math-object)
  )

(activate-rule 'r-matrix 'default)

;;}}}

;;}}}
