;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Splitting out methods from standard-math-objects:

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

(defmethod read-aloud  (( a-log a-log ))
  "Read aloud method for object log "
  (call-next-method))

(defmethod read-aloud  (( a-sin a-sin ))
  "Read aloud method for object sin "
  (call-next-method))

(defmethod read-aloud  (( alltex alltex ))
  "Read aloud method for object alltex "
  (read-aloud   "[l'aa_<50>t`ehkh]" )
  )

(defmethod leaf-p ((stackrel stackrel )) t)

(defmethod read-aloud  (( stackrel stackrel ))
  "Read aloud method for object stackrel "
  (afl:with-surrounding-pause (compute-pause stackrel)
    (read-aloud (argument stackrel 2 ))
    (with-reading-state (reading-state 'accent )
      (read-aloud (argument stackrel 1 ))))
  )
                                        ;(activate-rule 'stackrel 'default )

;;{{{ standard tex objects like set counter.

;;; Use  (argument object)  1 ...( argument
                        ;;; object 1)  in                         read-aloud
(defmethod read-aloud  (( chapterx chapterx ))
  "Read aloud method for object chapterx "
  (with-reading-state (reading-state 'annotation-voice )
    (read-aloud "Chapter: "))
  (with-reading-state (reading-state 'title-voice )
    (read-aloud (argument chapterx 1 ))))


(defmethod read-aloud  (( setcounter setcounter ))
  "Read aloud method for object setcounter "
  nil
  )

;;}}}

;;; french is not a math object, but ...

(defmethod read-aloud  (( french french ))
  "Read aloud method for object french "
  (afl:new-block
    (afl:local-set-state :french)
    (afl:local-set-state (reading-state 'annotation-voice ))
    (read-aloud (argument french 1  )))
  )


(defmethod read-aloud  (( inference inference ))
  "Read aloud method for object inference "
  (afl:new-block
    (read-aloud  (argument inference 1 ))
    (read-aloud " implies ")
    (read-aloud (argument inference 2 ))
    (afl:tts-force))
  )

(defmethod read-aloud  (( afl afl ))
  "Read aloud method for object afl "
  (with-reading-state (reading-state 'bold)
    (read-aloud " afl "))
  )
