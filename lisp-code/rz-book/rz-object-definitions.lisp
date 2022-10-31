
;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Mon Dec 21 09:23:42 EST 1992
;;; Contains  object definitions from Zippel's book
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 

;;{{{zippel book.
(define-text-object     :macro-name "keyi" 
  :number-args 1
  :processing-function keyi-expand 
  :object-name keyi
  :supers (document)
  )

;;; Use slots argument-1 ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( keyi keyi )) 
  "Read aloud method for object keyi "
(read-aloud  (argument keyi 1))
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
(defmethod read-aloud  (( addsymbol addsymbol )) 
  "Read aloud method for object addsymbol "
(read-aloud (argument 1 addsymbol))
(read-aloud "denotes ")
(read-aloud (argument 2 addsymbol ))
  )




;;}}}

