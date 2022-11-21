;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

;;; Contains state definitions for different fonts

(define-font-rule 'emphasize
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

(define-font-rule 'sans-seriph
    #'(lambda(state)
        (afl:generalized-afl-operator state
                                      '( afl:move-to afl:pitch-range
                                      250)
                                      '(afl:move-to  afl:quickness 100
                                      )
                                      '(afl:step-by  afl:average-pitch 2)
                                      '(afl:move-to afl:hat-rise 28)
                                      '(afl:move-to afl:stress-rise 42)
                                      '(afl:move-to afl:assertiveness 100)
                                      )
        )
  )
(define-font-rule 'bold
    #'(lambda(state)
        (afl:step-by  state
                      'afl:smoothness
                      2))
  )


(define-font-rule 'smallcaps
    #'(lambda(state)
        (afl:multi-step-by state
                           '(afl:breathiness 1))
        )
  )


(define-font-rule 'large 
    #'(lambda(state)
        (afl:step-by  state
                      'afl:head-size
                      2)
        )
  )
