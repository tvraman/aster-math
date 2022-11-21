;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :aster)

;;; Splitting object definitions and methods into separate files.

;;; Thu Dec 24 08:42:11 EST 1992
;;;  Standard math objects like log sin etc. can be specialized either
;;;  by defining subclasses of mathematical-function and making the
;;;  process-mathematical-function  create these subclasses, or by
;;;  using define-text-object and specifying the right superclass.
;;; I have used the former approach for specializing the big-operators
;;; to get integrals summations etc.
;;; The approach of using define-text-object seems equally good, so I
;;; am trying that here for the mathematical functions as and when I
;;; need them.
 

;;;
(define-text-object     :macro-name "log"
  :number-args 0
  :processing-function log-expand
  :precedence  mathematical-function
  :object-name a-log
  :supers (mathematical-function-name))

(define-text-object :macro-name "sin"
  :number-args 0
  :processing-function sin-expand
  :precedence  mathematical-function
  :object-name a-sin
  :supers (mathematical-function-name)
  )

(define-text-object :macro-name "over"
  :number-args 0
  :processing-function over-expand
  :precedence   tex-infix-operator
  :object-name over
  :supers (fraction binary-operator))

(define-text-object :macro-name "stackrel"
  :number-args 2
  :processing-function stackrel-expand
  :precedence  arrow-operator
  :object-name stackrel
  :supers (math-object))

;;{{{ standard tex objects like set counter.

(define-text-object :macro-name "chapterx"
  :number-args 1
  :processing-function chapterx-expand
  :precedence  nil
  :object-name chapterx
  :supers (document))

(define-text-object :macro-name "setcounter"
  :number-args 2
  :processing-function setcounter-expand
  :precedence  nil
  :object-name setcounter
  :supers (document))

;;}}}

;;; french is not a math object, but ...

(define-text-object :macro-name "french"
  :number-args 1
  :processing-function french-expand
  :precedence  nil
  :object-name french
  :supers (document))

(define-text-object :macro-name "inference"
  :number-args 2
  :processing-function inference-expand
  :precedence  arrow-operator
  :object-name inference
  :supers ( binary-operator)
  :children-are-called (list "premise" "conclusion"))

(define-text-object :macro-name "afl"
  :number-args 0
  :processing-function afl-expand
  :precedence  nil
  :object-name afl
  :supers (document)
  :children-are-called nil)


(define-text-object :macro-name "alltex" 
  :number-args 0
  :processing-function alltex-expand 
  :precedence  nil 
  :object-name alltex
  :supers (document)
  )
