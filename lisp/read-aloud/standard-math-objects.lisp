;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Thu Dec 24 08:42:11 EST 1992
;;;  Standard math objects like log sin etc. can be specialized either
;;;  by defining subclasses of mathematical-function and making the
;;;  process-mathematical-function  create these subclasses, or by
;;;  using define-text-object and specifying the right superclass.
;;; I have used the former approach for specializing the big-operators
;;; to get integrals summations etc.
;;; The approach of using define-text-object seems equally good, so I
;;; am trying that here for the mathematical functions as and when I
;;; need them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
(define-text-object     :macro-name "log" 
  :number-args 0
  :processing-function log-expand 
  :precedence  mathematical-function
  :object-name a-log
  :supers (mathematical-function-name)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( a-log a-log )) 
  "Read aloud method for object log "
  (call-next-method)
  )


(define-text-object :macro-name "sin" 
  :number-args 0
  :processing-function sin-expand 
  :precedence  mathematical-function 
  :object-name a-sin
  :supers (mathematical-function-name)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( a-sin a-sin )) 
  "Read aloud method for object sin "
  (call-next-method)
  )



(define-text-object :macro-name "over" 
  :number-args 0
  :processing-function over-expand 
  :precedence   tex-infix-operator 
  :object-name over
  :supers (fraction binary-operator)
  )

(define-text-object :macro-name "alltex" 
  :number-args 0
  :processing-function alltex-expand 
  :precedence  nil 
  :object-name alltex
  :supers (document)
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( alltex alltex )) 
  "Read aloud method for object alltex "
  (read-aloud   "[l'aa_<50>t`ehkh]" ) 
  )

(define-text-object :macro-name "stackrel" 
  :number-args 2
  :processing-function stackrel-expand 
  :precedence  arrow-operator  
  :object-name stackrel
  :supers (math-object)
  )
(defmethod leaf-p ((stackrel stackrel )) t)
;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( stackrel stackrel )) 
  "Read aloud method for object stackrel "
  (afl:with-surrounding-pause (compute-pause stackrel)
    (read-aloud (argument 2 stackrel ))
    (with-reading-state (reading-state 'accent )
      (read-aloud (argument 1 stackrel ))))
  )
                                        ;(activate-rule 'stackrel 'default )


;;{{{ standard tex objects like set counter.
;;; should go in a separate file later

(define-text-object :macro-name "chapterx" 
  :number-args 1
  :processing-function chapterx-expand 
  :precedence  nil 
  :object-name chapterx
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( chapterx chapterx )) 
  "Read aloud method for object chapterx "
  (with-reading-state (reading-state 'annotation-voice )
    (read-aloud "Chapter: "))
  (with-reading-state (reading-state 'title-voice )
    (read-aloud (argument 1 chapterx )))
  )


(define-text-object :macro-name "setcounter" 
  :number-args 2
  :processing-function setcounter-expand 
  :precedence  nil 
  :object-name setcounter
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( setcounter setcounter )) 
  "Read aloud method for object setcounter "
  nil
  )




;;}}}



;;; french is not a math object, but ...

(define-text-object :macro-name "french" 
  :number-args 1
  :processing-function french-expand 
  :precedence  nil 
  :object-name french
  :supers (document)
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud 
(defmethod read-aloud  (( french french )) 
  "Read aloud method for object french "
  (afl:new-block
   (afl:local-set-state :french)
   (afl:local-set-state (reading-state 'annotation-voice ))
   (read-aloud (argument 1 french  )))
  )


(define-text-object :macro-name "inference" 
  :number-args 2
  :processing-function inference-expand 
  :precedence  arrow-operator 
  :object-name inference
  :supers ( binary-operator)
  :children-are-called (list "premise" "conclusion")
  )

;;; Use  (argument object)  1 ...( argument
                        ;;; object 2)  in                         read-aloud 
(defmethod read-aloud  (( inference inference )) 
  "Read aloud method for object inference "
  (afl:new-block
   (read-aloud  (argument 1 inference ))
   (read-aloud " implies ")
   (read-aloud (argument 2 inference ))
   (afl:tts-force))
  )




(define-text-object :macro-name "afl" 
  :number-args 0
  :processing-function afl-expand 
  :precedence  nil 
  :object-name afl
  :supers (document)
  :children-are-called nil
  )

;;; Object has 0 slots 
(defmethod read-aloud  (( afl afl )) 
  "Read aloud method for object afl "
  (with-reading-state (reading-state 'bold)
    (read-aloud " afl "))
  )








