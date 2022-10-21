;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)



;;; { xis from vavasis book

(define-text-object     :macro-name "xis" 
  :number-args 0
  :processing-function xis-expand 
  :object-name xis
  :supers (math)
  )

(defmethod read-aloud  (( xis xis )) 
  "Read aloud method for object xis "
  (read-aloud " x eye star" )
  )

(define-text-object     :macro-name "xio" 
  :number-args 0
  :processing-function xio-expand 
  :precedence  nil 
  :object-name xio
  :supers (math)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( xio xio )) 
  "Read aloud method for object xio "
  (read-aloud "x eye hat")
              )
  
  
  
;;; }
;;; { nph 
  
  (define-text-object       :macro-name "nph" 
    :number-args 0
    :processing-function nph-expand 
    :object-name nph
    :supers (document)
    )
  
  (defmethod read-aloud  (( nph nph )) 
    "Read aloud method for object nph "
    (afl:synchronize-and-play  "/home/raman/sounds/cues/bark.au"
                                   :background-flag t)
    (read-aloud " np hard ")
    )
  
  (define-text-object       :macro-name "ze" 
    :number-args 0
    :processing-function ze-expand 
    :precedence  nil 
    :object-name ze
    :supers (math-number)
    )
  
;;; Object has 0 slots 
  (defmethod read-aloud  (( ze ze )) 
    "Read aloud method for object ze "
    (read-aloud "zero")
    )
  
  (define-text-object       :macro-name "xs" 
    :number-args 0
    :processing-function xs-expand 
    :precedence  nil 
    :object-name xs
    :supers (math)
    )
  
;;; Object has 0 slots 
  (defmethod read-aloud  (( xs xs )) 
    "Read aloud method for object xs "
    (read-aloud "x star")
    )
  
  (define-text-object       :macro-name "ddfdxi" 
    :number-args 0
    :processing-function ddfdxi-expand 
    :precedence  nil 
    :object-name ddfdxi
    :supers (ordinary)
    )
  
;;; Object has 0 slots 
  (defmethod read-aloud  (( ddfdxi ddfdxi )) 
    "Read aloud method for object ddfdxi "
    (read-aloud  "partial derivative of f with respect to x i")
  )

(define-text-object     :macro-name "dfdxi" 
  :number-args 0
  :processing-function dfdxi-expand 
  :precedence  nil 
  :object-name dfdxi
  :supers (ordinary)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( dfdxi dfdxi )) 
  "Read aloud method for object dfdxi "
  (read-aloud "partial derivative of f with respect to x i")
  )









;;; }



;;; {cross references

(define-text-object     :macro-name "sref" 
  :number-args 1
  :processing-function sref-expand 
  :precedence  nil 
  :object-name section-ref
  :supers (cross-ref)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 


(define-text-object :macro-name "cref" 
  :number-args 1
  :processing-function cref-expand 
  :precedence  nil 
  :object-name chapter-ref
  :supers (cross-ref)
  )

(define-text-object :macro-name "figref" 
  :number-args 1
  :processing-function fig-ref-expand 
  :precedence  nil 
  :object-name fig-ref
  :supers (cross-ref)
  :children-are-called nil
  )


(define-text-object :macro-name "eqref" 
  :number-args 1
  :processing-function eq-ref-expand 
  :precedence  nil 
  :object-name eq-ref
  :supers (cross-ref)
  :children-are-called nil
  )


(define-text-object :macro-name "tabref" 
  :number-args 1
  :processing-function tab-ref-expand 
  :precedence  nil 
  :object-name tab-ref
  :supers (cross-ref)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument









;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 




(define-text-object     :macro-name "eref" 
  :number-args 1
  :processing-function eref-expand 
  :precedence  nil 
  :object-name eref
  :supers (cross-ref)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 

(define-text-object :macro-name "aref" 
  :number-args 1
  :processing-function aref-expand 
  :precedence  nil 
  :object-name appendix-ref
  :supers (cross-ref)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 




;;; }

(define-text-object :macro-name "bmu" 
  :number-args 0
  :processing-function bmu-expand 
  :precedence  nil 
  :object-name bmu
  :supers (ordinary)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( bmu bmu )) 
  "Read aloud method for object bmu "
(setf (contents bmu) "bold mu" )
(call-next-method)
  )

(activate-rule  'bmu 'default)


(define-text-object :macro-name "bref" 
  :number-args 1
  :processing-function bref-expand 
  :precedence  nil 
  :object-name b-ref
  :supers (document)
  :children-are-called nil
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( b-ref b-ref )) 
  "Read aloud method for object b-ref "
(read-aloud "refer to the book, ")
(read-aloud (argument 1 b-ref ))
(afl:comma-intonation)
(afl:force-speech)
  )


