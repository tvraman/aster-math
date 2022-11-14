;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Mon Dec 21 09:31:14 EST 1992
;;; Object definitions from cs611 notes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; { subst

;;; \newcommand{\subst}[3]{#1[#2/#3]}
(define-text-object :macro-name "subst"
  :number-args 3
  :processing-function subst-expand
  :precedence mathematical-function
  :object-name a-subst
  :supers (math-object))

;;; }
;;; { abstraction

;;; \renewcommand{\abs}[2]{\lambda(#1. #2)}
(define-text-object     :macro-name "abs" 
  :number-args  2
  :processing-function  abs-expand
  :object-name abstraction
  :supers (math-object)
  )


;;; }
;;; { application

(define-text-object     :macro-name "ap" 
  :number-args 2
  :processing-function ap-expand 
  :object-name application
  :supers (math-object)
  )





;;; }

