;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; {cn15a.tex

;;;  Contains the equivalent audio macros for cn15 which looks like it
;;;  has been well written.

;;;
(define-text-object     :macro-name "alphaeq"
  :number-args 0
  :processing-function alphaeq-expand
  :object-name alpha-equality
  :precedence relational-operator
  :supers (relational-operator)
  )



(define-text-object     :macro-name "walksa"
  :number-args 0
  :processing-function walksa-expand
  :object-name walksa
  :supers (math)
  )

(define-text-object     :macro-name "alphaone"
  :number-args 0
  :processing-function alphaone-expand
  :object-name alphaone
  :supers (math)
  )

(define-text-object     :macro-name "betaone"
  :number-args 0
  :processing-function betaone-expand
  :object-name betaone
  :supers (math)
  )

(define-text-object     :macro-name "abmany"
  :number-args 0
  :processing-function abmany-expand
  :object-name abmany
  :supers (math)
  )

(define-text-object     :macro-name "displaystyle"
  :number-args 1
  :processing-function displaystyle-expand
  :object-name displaystyle
  :supers (math)
  )

;;; }

;;; { cn24 macros.

;;; \newcommandDerivII[3]...
(define-text-object     :macro-name "DerivII"
  :number-args 3
  :processing-function derivation-ii-expand
  :object-name derivation-ii
  :supers (math)
  )

;;; Use slots argument ... 1 argument-3 in                         read-aloud

(define-reading-state 'yields
    #'(lambda(state)
        (afl:step-by state 'afl:average-pitch 1)))

;;; \newcommand[2]DerivI...
(define-text-object     :macro-name "DerivI"
  :number-args 2
  :processing-function derivation-i-expand
  :object-name derivation-i
  :supers (math)
  )

;;; }
;;; { pred from solution to problem set 2 cs611

(define-text-object     :macro-name "pred"
  :number-args 0
  :processing-function pred-expand
  :object-name pred
  :supers (math)
  )

;;; Object has 0 slots

;;; }
