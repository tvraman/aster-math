;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)

;;{{{ xis from vavasis book

(define-text-object     :macro-name "xis"
  :number-args 0
  :processing-function xis-expand
  :object-name xis
  :supers (math))

(define-text-object     :macro-name "xio"
  :number-args 0
  :processing-function xio-expand
  :precedence  nil
  :object-name xio
  :supers (math)
  )

 




;;}}}
;;{{{ nph

(define-text-object       :macro-name "nph"
  :number-args 0
  :processing-function nph-expand
  :object-name nph
  :supers (document)
  )



(define-text-object       :macro-name "ze"
  :number-args 0
  :processing-function ze-expand
  :precedence  nil
  :object-name ze
  :supers (math-number)
  )

 


(define-text-object       :macro-name "xs"
  :number-args 0
  :processing-function xs-expand
  :precedence  nil
  :object-name xs
  :supers (math)
  )

 


(define-text-object       :macro-name "ddfdxi"
  :number-args 0
  :processing-function ddfdxi-expand
  :precedence  nil
  :object-name ddfdxi
  :supers (ordinary)
  )

 

(define-text-object     :macro-name "dfdxi"
  :number-args 0
  :processing-function dfdxi-expand
  :precedence  nil
  :object-name dfdxi
  :supers (ordinary)
  )

 

;;}}}

;;{{{cross references

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

;;}}}

(define-text-object :macro-name "bmu"
  :number-args 0
  :processing-function bmu-expand
  :precedence  nil
  :object-name bmu
  :supers (ordinary)
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
