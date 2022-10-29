;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; rearrange dimensions for dectalk:

(rearrange-dimensions-for-dectalk)
;;; first define points in speech space

(define-point-in-speech-space)

;;; Export all dimension names

(eval `(export ',*list-of-speech-dimensions*))
;;; This file defines the standard voices as points in the speech
;;; space using the function define-standard-voice

;;; set up standard voices.

(define-standard-voice 'paul
    '(head-size(:value 100 :step-size 5))
  '(speech-rate (:value  180 :step-size 25))
  '(breathiness (:value 0 :step-size 10))
  '(lax-breathiness (:value 0 :step-size 25))
  '(smoothness (:value 3 :step-size 20))
  '(richness (:value 70 :step-size 20))
  '(laryngilization (:value 0 :step-size 10))
  '(baseline-fall (:value 18 :step-size 10))
  '(hat-rise (:value 18 :step-size 10))
  '(stress-rise (:value 32 :step-size 20))
  '(assertiveness (:value 100 :step-size 25))
  '(quickness (:value 40 :step-size 10))
  '(average-pitch (:value 122 :step-size 10))
  '(pitch-range (:value 100 :step-size 10))
  #+multivoice '(loudness (:value 86 :step-size 5))
  )

(define-standard-voice 'betty
    '(head-size (:value 100 :step-size 5))
  '(speech-rate (:value 180 :step-size 20))
  '(breathiness (:value 0 :step-size 10))
  '(lax-breathiness (:value 80 :step-size 25))
  '(smoothness (:value 4 :step-size 20))
  '(richness (:value 40 :step-size 10))
  '(laryngilization (:value 0 :step-size 10))
  '(baseline-fall (:value 0 :step-size 10))
  '(hat-rise (:value 14 :step-size 10))
  '(stress-rise (:value 20 :step-size 20))
  '(assertiveness (:value 35 :step-size 25))
  '(quickness (:value 55 :step-size 10))
  '(average-pitch (:value 208 :step-size 10))
  '(pitch-range (:value 140 :step-size 10))
  #+multivoice '(loudness (:value 81 :step-size 5))
  )

(define-standard-voice 'harry
    '(head-size (:value 115 :step-size 5))
  '(speech-rate (:value 180 :step-size 20))
  '(breathiness (:value 0 :step-size 10))
  '(lax-breathiness (:value 0 :step-size 25))
  '(smoothness (:value 12 :step-size 20))
  '(richness (:value 86 :step-size 10))
  '(laryngilization (:value 0 :step-size 10))
  '(baseline-fall (:value 9 :step-size 10))
  '(hat-rise (:value 20 :step-size 10))
  '(stress-rise (:value 30 :step-size 20))
  '(assertiveness (:value 100 :step-size 25))
  '(quickness (:value 10 :step-size 10))
  '(average-pitch (:value 89 :step-size 10))
  '(pitch-range (:value 80 :step-size 10))
  #+multivoice  '(loudness (:value 81 :step-size 5))
  )

(define-standard-voice 'dennis
    '(head-size (:value 105 :step-size 5))
  '(speech-rate (:value 180 :step-size 20))
  '(breathiness (:value 38 :step-size 10))
  '(lax-breathiness (:value 70 :step-size 25))
  '(smoothness (:value 100 :step-size 20))
  '(richness (:value 0 :step-size 10))
  '(laryngilization (:value 0 :step-size 10))
  '(baseline-fall (:value 9 :step-size 10))
  '(hat-rise (:value 20 :step-size 10))
  '(stress-rise (:value 22 :step-size 20))
  '(assertiveness (:value 100 :step-size 25))
  '(quickness (:value 50 :step-size 10))
  '(average-pitch (:value 110 :step-size 10))
  '(pitch-range (:value 135 :step-size 10))
  #+multivoice '(loudness (:value 84 :step-size 5))
  )

(define-standard-voice 'kid
    '(head-size (:value 80 :step-size 5))
  '(speech-rate (:value 180 :step-size 20))
  '(breathiness (:value 47 :step-size 10))
  '(lax-breathiness (:value 75 :step-size 25))
  '(smoothness (:value 5 :step-size 20))
  '(richness (:value 40 :step-size 10))
  '(laryngilization (:value 0 :step-size 10))
  '(baseline-fall (:value 0 :step-size 10))
  '(hat-rise (:value 20 :step-size 10))
  '(stress-rise (:value 22 :step-size 20))
  '(assertiveness (:value 65 :step-size 25))
  '(quickness (:value 50 :step-size 10))
  '(average-pitch (:value 306 :step-size 10))
  '(pitch-range (:value 210 :step-size 10))
  #+multivoice  '(loudness (:value 73 :step-size 5))
  )
