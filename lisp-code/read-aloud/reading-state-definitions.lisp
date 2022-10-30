;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)


;;; Sets up association between different reading states and afl
;;; setters

(define-reading-state 'annotation-voice
    #'(lambda(state)
        (afl:step-by  state
                      'afl:smoothness
                      2))
  )


(define-reading-state 'title-voice
    #'(lambda(state)
        (afl:step-by  state 
                      'afl:head-size
                      1))
  )
(define-reading-state 'abstract
    #'(lambda(state)
        (afl:step-by  state
                      'afl:head-size 2 ))
  )

(define-reading-state 'list-environment-voice
    #'(lambda(state)
        (afl:step-by  state
                      'afl:average-pitch
                      2))
  )

(define-reading-state 'quotation-voice
    #'(lambda(state)
        (afl:step-by state 
                     'afl:head-size
                     2))
  )
(define-reading-state 'verbatim-voice
    #'(lambda(state)
        (afl:multi-move-to
         state
         '(afl:pitch-range 0)
         '(afl:hat-rise 3)
         '(afl:stress-rise 3)
         )
        )
  )

;;; According to both Jim Davis and David Gries the following choice
;;; of dimensions for subscripting and superscripting should be
;;; interchanged.  While playing with the code, these may be swapped
;;; with swap-reading-states so what this file says and what lisp is
;;; actually doing may be different. (at least until I stabilize on 
;;; one or the other)
(define-reading-state 'subscript 
    #'(lambda(state)
        (afl:generalized-afl-operator state
                                      '(afl:step-by afl:average-pitch -1.5)
                                      '(afl:step-by afl:head-size .5)
                                      '(afl:scale-by  afl:average-pitch .5
                                        :slot afl:step-size)
                                      '(afl:scale-by afl:head-size .5
                                        :slot afl:step-size)
                                      )
        )
  )

(define-reading-state 'superscript
    #'(lambda(state)
        (afl:generalized-afl-operator  state
                                       '(afl:step-by afl:average-pitch 1.5)
                                       '(afl:step-by afl:head-size -.5)
                                       '(afl:scale-by  afl:average-pitch .5
                                         :slot afl:step-size)
                                       '(afl:scale-by afl:head-size .5
                                         :slot afl:step-size)
                                       )
        )
  )

(define-reading-state 'left-superscript
    #'(lambda(state)
        (afl:multi-step-by state
                           '(afl:average-pitch -1)
                           '(afl:head-size 1)
                           )
        )
  )

(define-reading-state 'left-subscript
    #'(lambda(state)
        (afl:multi-step-by state
                           '(afl:average-pitch -1)
                           '(afl:head-size -1)
                           )
        )
  )

(define-reading-state 'accent
    #'(lambda(state)
        (afl:step-by state
                     'afl:head-size -1)
        )
  )

(define-reading-state 'underbar
    #'(lambda(state)
        (afl:step-by state
                     'afl:head-size 1))
  )

(define-reading-state 'math
    #'(lambda(state)
        (afl:multi-scale-by state
                            '(afl:average-pitch 6
                              :slot afl:step-size)
                            '(afl:head-size  2
                              :slot afl:step-size)
                            ))
  )


(define-reading-state 'children
    #'(lambda(state)
        (afl:multi-step-by state
                           '(afl:smoothness 2)
                           '(afl:richness -1)
                           ;'(afl:loudness 2)
                           '(afl:quickness 1)
                           '(afl:hat-rise 2)
                           '(afl:stress-rise 2)
                           ))
  )

(define-reading-state 'subformula
    #'(lambda(state)
        (afl:step-by state
                     'afl:average-pitch   1)))

(define-reading-state 'fraction
    #'(lambda(state)
        (afl:step-by state
                     'afl:hat-rise 1)))

(define-reading-state  'fraction-numerator
    #'(lambda(state)
        (afl:step-by state 
                     'afl:stress-rise 4)))

(define-reading-state 'fraction-denominator
    #'(lambda(state)
        (afl:step-by state 
                     'afl:stress-rise
                     -4)))


(define-reading-state 'center
    #'(lambda(state)
        (afl:generalized-afl-operator  state
                                       '(afl:move-to afl:pitch-range 200)
                                       '(afl:move-to afl:quickness  100)
                                       '(afl:step-by afl:stress-rise 1)
                                       ))
  )

;;; for dtrace audio display:

(define-reading-state 'variable-name 
    #'(lambda(state)
        (afl:step-by  state
                      'afl:smoothness
                      2))
  )

(define-reading-state 'variable-value
    #'(lambda(state)
        (declare (ignore state))
        (afl:get-point-in-speech-space 'afl:harry))
  )

(define-reading-state 'return-value
    #'(lambda(state)
        (declare (ignore state))
        (afl:get-point-in-speech-space 'afl:betty))
  )


(define-reading-state 'footnote
    #'(lambda(state)
        (afl:generalized-afl-operator state 
                                      ;'(afl:move-to afl:left-volume  0)
                                      ;'(afl:move-to  afl:right-volume 100)
                                      '(afl:scale-by afl:speech-rate 1.5)
                                      )
        )
  )


(define-reading-state 'emphasize
    #'(lambda(state)
        (afl:generalized-afl-operator state
                                      '( afl:move-to afl:pitch-range  250)
                                      '(afl:step-by  afl:average-pitch 2)
                                      '(afl:move-to afl:hat-rise 28)
                                      '(afl:move-to afl:stress-rise 42)
                                      '(afl:move-to afl:assertiveness 100)
                                      )
        )
  )
(define-reading-state 'bold
    #'(lambda(state)
        (afl:step-by state
                     'afl:smoothness 2))
  )

 
 
