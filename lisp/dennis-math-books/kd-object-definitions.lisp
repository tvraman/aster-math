;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Sat Mar  6 15:14:41 EST 1993
;;; Contains objects from math books from Keith Dennis

;;{{{ Kaplansky:

(define-text-object :macro-name "e"
  :number-args 0
  :processing-function kd-e-expand
  :precedence  nil
  :object-name kd-e
  :supers (document)
  )


(define-text-object :macro-name "snoindent"
  :number-args 0
  :processing-function snoindent-expand
  :precedence  nil
  :object-name snoindent
  :supers (document)
  )


(define-text-object :macro-name "pno"
  :number-args 1
  :processing-function pno-expand
  :precedence  nil
  :object-name pno
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud

(define-text-object :macro-name "seject"
  :number-args 0
  :processing-function seject-expand
  :precedence  nil
  :object-name seject
  :supers (document)
  )


(define-text-object :macro-name "h"
  :number-args 0
  :processing-function kd-h-expand
  :precedence  nil
  :object-name kd-h
  :supers (document)
  )


;;}}}

;;{{{ galois
;;; a font macro
(define-text-object :macro-name "fontbi"
  :number-args 0
  :processing-function fontbi-expand
  :precedence  nil
  :object-name fontbi
  :supers (document)
  )


(define-text-object :macro-name "leqno"
  :number-args 0
  :processing-function leqno-expand
  :precedence  nil
  :object-name leqno
  :supers (document)
  )



;;}}}
