;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Some literature hacks to show off afl and recognizer:

(define-text-object     :macro-name "characterone" 
  :number-args 1
  :processing-function character-one-expand 
  :object-name character-one
  :supers (document)
  )

;;; Use slots argument-1 ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( character-one character-one )) 
  "Read aloud method for object character-one "
  (with-reading-state  (reading-state 'annotation-voice) 
    (read-aloud (argument character-one 1  )))
  (afl:local-set-state (reading-state 'character-one))
  )

(define-text-object     :macro-name "charactertwo" 
  :number-args 1
  :processing-function character-two-expand 
  :object-name character-two
  :supers (document)
  )

;;; Use slots argument- ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( character-two character-two )) 
  "Read aloud method for object character-two "
  (with-reading-state (reading-state 'annotation-voice) 
    (read-aloud (argument character-two 1 )))
  (afl:local-set-state (reading-state 'character-two))
  )

(define-text-object     :macro-name "characterthree" 
  :number-args 1
  :processing-function character-three-expand 
  :object-name character-three
  :supers (document)
  )

;;; Use slots argument-1 ... argument-1 in                         read-aloud 
(defmethod read-aloud  (( character-three character-three )) 
  "Read aloud method for object character-three "
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud (argument character-three 1 )))
  (afl:local-set-state (reading-state 'character-three))
  )

(define-reading-state 'character-one
    #'(lambda(state)
        (afl:multi-move-to state
                           '(afl:left-volume 100)
                           '(afl:right-volume 0)))
  )

(define-reading-state 'character-two
    #'(lambda(state)
        (afl:multi-move-to  state
                            '(afl:left-volume 0 )
                            '(afl:right-volume 100)))
  )

(define-reading-state 'character-three
    #'(lambda(state)
        (declare (ignore state))
        (let ((new-state 
               (afl:get-point-in-speech-space 'afl:harry )))
          (afl:multi-move-to new-state
                             '(afl:left-volume 50)
                             '(afl:right-volume 50 )))))

