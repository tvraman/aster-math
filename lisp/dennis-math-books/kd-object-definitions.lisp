;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Object has 0 slots 
(defmethod read-aloud  (( kd-e kd-e )) 
  "Read aloud method for object kd-e "
nil
  )




(define-text-object :macro-name "snoindent" 
  :number-args 0
  :processing-function snoindent-expand 
  :precedence  nil 
  :object-name snoindent
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( snoindent snoindent )) 
  "Read aloud method for object snoindent "
nil
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
(defmethod read-aloud  (( pno pno )) 
  "Read aloud method for object pno "
(read-aloud "end of page ")
(read-aloud (argument pno 1))
(afl:tts-queue  "[_.]")
(afl:tts-icon *newline-cue*)
  )
(define-text-object :macro-name "seject" 
  :number-args 0
  :processing-function seject-expand 
  :precedence  nil 
  :object-name seject
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( seject seject )) 
  "Read aloud method for object seject "
nil
  )

(define-text-object :macro-name "h" 
  :number-args 0
  :processing-function kd-h-expand 
  :precedence  nil 
  :object-name kd-h
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( kd-h kd-h )) 
  "Read aloud method for object kd-h "
(afl:tts-queue "[_.]")
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

;;; Object has 0 slots 
(defmethod read-aloud  (( fontbi fontbi )) 
  "Read aloud method for object fontbi "
nil
  )


(define-text-object :macro-name "leqno" 
  :number-args 0
  :processing-function leqno-expand 
  :precedence  nil 
  :object-name leqno
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( leqno leqno )) 
  "Read aloud method for object leqno "
(read-aloud "equation ")
  )




;;}}}
