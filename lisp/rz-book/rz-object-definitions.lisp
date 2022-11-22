;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)

;;; Mon Dec 21 09:23:42 EST 1992
;;; Contains  object definitions from Zippel's book
 

;;{{{zippel book.
(define-text-object     :macro-name "keyi"
  :number-args 1
  :processing-function keyi-expand
  :object-name keyi
  :supers (document)
  )

;;}}}

;;{{{ addsymbol:

(define-text-object :macro-name "addsymbol"
  :number-args 2
  :processing-function addsymbol-expand
  :precedence  nil
  :object-name addsymbol
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud

;;}}}
