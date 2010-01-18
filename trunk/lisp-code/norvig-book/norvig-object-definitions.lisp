;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

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
(defmethod read-aloud  (( idx-term idx-term )) 
  "Read aloud method for object idx-term "
  (with-reading-state (reading-state 'emphasize)
(read-aloud (argument 1 idx-term )))
  )




(define-new-environment :env-name "asis" :class-name asis)

(defmethod read-aloud ((asis asis))
  "Read out contents of asis"
  (afl:with-pronunciation-mode (:mode :lisp)
    (read-aloud (contents asis )))
  )

(define-text-object :macro-name "askip" 
  :number-args 0
  :processing-function askip-expand 
  :precedence  nil 
  :object-name askip
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( askip askip )) 
  "Read aloud method for object askip "
nil
  )


(define-text-object :macro-name "idx" 
  :number-args 0
  :processing-function norvig-idx-expand 
  :precedence  nil 
  :object-name norvig-idx
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( norvig-idx norvig-idx )) 
  "Read aloud method for object norvig-idx "
nil
  )


