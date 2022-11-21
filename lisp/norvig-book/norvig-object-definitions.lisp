;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Tue Feb 23 09:31:08 EST 1993
;;;

(define-text-object :macro-name "idx" 
  :number-args 1
  :processing-function idx-expand 
  :precedence  nil 
  :object-name idx-term
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 





(define-new-environment :env-name "asis" :class-name asis)



(define-text-object :macro-name "askip" 
  :number-args 0
  :processing-function askip-expand 
  :precedence  nil 
  :object-name askip
  :supers (document)
  )

 



(define-text-object :macro-name "idx" 
  :number-args 0
  :processing-function norvig-idx-expand 
  :precedence  nil 
  :object-name norvig-idx
  :supers (document))

 



