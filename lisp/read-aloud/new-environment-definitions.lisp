;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)
;;; 
;;; Fri Apr 30 10:00:14 EDT 1993
;;; Define new environments here:
;;; 

(define-new-environment  :env-name "theorem"  :class-name theorem)
(define-new-environment :env-name "minipage" :class-name mini-page)
(define-new-environment  :env-name "table"  :class-name table)
(define-new-environment  :env-name "lemma"  :class-name lemma)


(define-new-environment  :env-name "corollary"  :class-name corollary) 

(define-new-environment  :env-name "definition"  :class-name
                         definition)

(define-new-environment  :env-name "proof"  :class-name proof)

(define-new-environment  :env-name "conjecture"  :class-name conjecture)
(define-new-environment :env-name  "algorithm"  :class-name algorithm)
(define-new-environment  :env-name "LVerbatim"  :class-name
                         l-verbatim)
(define-new-environment :env-name "figure" :class-name figure)
(define-new-environment :env-name "lisp:documentation"
  :class-name lisp-documentation)
(define-new-environment :env-name "thesisfigure" :class-name
                        thesis-figure :supers  (figure))


(define-new-environment :env-name "titlepage"  :class-name title-page)
(define-new-environment :env-name "copyrightpage"  :class-name
                        copyright-page)
(define-new-environment :env-name "dedication" :class-name dedication)
(define-new-environment :env-name  "acknowledgements"
  :class-name acknowledgements)
(define-new-environment :env-name  "biosketch"
  :class-name biographical-sketch )
(define-new-environment :env-name  "displaytext" :class-name  display-text )
