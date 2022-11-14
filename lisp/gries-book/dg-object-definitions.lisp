;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Tue May  4 12:43:05 EDT 1993
 ;;; Objects from gries book:

;;{{{ constraint delimiters:

(define-math-delimiter '(math-cs "cc")   '(math-cs "rb" )
  "constraint-brackets")

(defclass constraint-brackets (delimited-expression) ())

(activate-rule 'constraint-brackets 'default)

;;}}}

(define-text-object :macro-name "smallsum"
  :number-args 0
  :processing-function smallsum-expand
  :precedence  big-operator
  :object-name small-summation
  :supers (summation)
  )

(define-text-object-with-label :macro-name "theorem"
  :processing-function new-theorem-expand
  :precedence  nil
  :object-name new-theorem
  :supers (defined-text-object-with-label))



(define-text-object-with-label :macro-name "definition"
  :processing-function new-definition-expand
  :precedence  nil
  :object-name new-definition
  :supers (defined-text-object-with-label))

(define-text-object-with-label :macro-name "boxedequation"
  :processing-function boxed-equation-expand
  :precedence  nil
  :object-name  boxed-equation
  :supers (defined-text-object-with-label))

(define-text-object :macro-name "mult"
  :number-args 0
  :processing-function mult-expand
  :precedence  multiplication
  :object-name mult
  :supers (binary-operator)
  )

(define-text-object :macro-name "lefteqn"
  :number-args 1
  :processing-function lefteqn-expand
  :precedence  nil
  :object-name lefteqn
  :supers (math)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud

(define-text-object-with-label :macro-name "history"
  :processing-function history-expand
  :precedence  nil
  :object-name historical-note
  :supers (defined-text-object-with-label)
  )
