;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; export all dimension names.

;;; This file has the user definition of speech space, ie: set up list
;;; of dimensions, specify default values etc.
;;; Each fold contains assignments to one global table

;;{{{ *list-of-speech-dimensions*

;;; Function: REARRANGE-DIMENSIONS-FOR-DECTALK               Author: raman
;;; Created: Sun Aug 16 12:33:28 1992

(defun rearrange-dimensions-for-dectalk ()
  "Rearrange dimensions in list-of-speech-dimensions for dectalk so that voice
is at front"
  (if (find 'voice *list-of-speech-dimensions*)
      (setf *list-of-speech-dimensions*
            (cons 'voice
                  (remove 'voice *list-of-speech-dimensions*)))))

;;; First initialize by setting list of dimensions to nil

(setf *list-of-speech-dimensions* nil)
;;; At present for the dectalk the order in which dimensions are added
;;; is important.  I will need to fix this. The problem is that the
;;; voice dimension has to be set first on the dectalk as setting this
;;; after setting something else clobbers earlier settings.
;;; stereo present on multivoice:

;;; The Express does not provide a earphone port.
;;; So we have no right volume.
;;; The express does have tones.
;;; So if we define the synthesizer code for right-volume to be
;;; :tone 500,
;;; then setting right volume will actually produce a beep, with the duration
;;; determined by the value to which we would have set right volume.
;;; What a kluge:-
;(add-dimension 'left-volume)
;(add-dimension 'right-volume)
;;; this went away after dectalk 3:
;(add-dimension 'loudness)
;;; common dimensions to multivoice and express
(add-dimension 'lax-breathiness)
(add-dimension 'average-pitch)
(add-dimension 'pitch-range)
(add-dimension 'breathiness)
(add-dimension 'head-size)
(add-dimension 'speech-rate)
(add-dimension 'smoothness)
(add-dimension 'richness)
(add-dimension 'laryngilization)
(add-dimension 'baseline-fall)
(add-dimension 'hat-rise)
(add-dimension 'stress-rise)
(add-dimension 'assertiveness )
(add-dimension 'quickness)
(add-dimension 'voice)

;;}}}
;;{{{ define-synthesizer-code

;;; defining some synthesizer codes. dectalk specific

;(define-synthesizer-code 'left-volume " :vs ")
;#+multivoice (define-synthesizer-code 'right-volume " :ve ")
;#+express  (define-synthesizer-code 'right-volume ":sync  :tone 500 ")
(define-synthesizer-code 'lax-breathiness " :dv lx ")
(define-synthesizer-code 'voice ":n")
(define-synthesizer-code 'head-size ":dv hs ")
(define-synthesizer-code 'breathiness " :dv br ")
(define-synthesizer-code 'speech-rate " :ra ")
(define-synthesizer-code 'average-pitch ":dv ap ")
(define-synthesizer-code 'pitch-range ":dv pr ")
(define-synthesizer-code 'smoothness " :dv sm ")
(define-synthesizer-code 'richness ":dv ri ")
(define-synthesizer-code 'laryngilization " :dv la ")
(define-synthesizer-code 'baseline-fall " :dv bf ")
(define-synthesizer-code 'hat-rise " :dv hr ")
(define-synthesizer-code 'stress-rise " :dv sr ")
(define-synthesizer-code 'assertiveness " :dv as ")
(define-synthesizer-code 'quickness " :dv qu ")
#+multivoice (define-synthesizer-code 'loudness ":dv lo ")
#+express  (define-synthesizer-code 'loudness ":dv g5 ")
;;}}}
;;{{{ define-default-value

;;; Some default values
(define-default-value 'head-size 100)
(define-default-value 'speech-rate 180)
(define-default-value 'lax-breathiness 0)
(define-default-value 'average-pitch  122)
(define-default-value 'pitch-range 100)
(define-default-value 'breathiness 0)
(define-default-value 'left-volume 50)
(define-default-value 'right-volume 50)
(define-default-value 'smoothness 0)
(define-default-value 'richness 100)
(define-default-value 'laryngilization 0)
(define-default-value 'baseline-fall 0)
(define-default-value 'hat-rise 10)
(define-default-value 'stress-rise 5)
(define-default-value 'assertiveness 50)
(define-default-value 'quickness 0)
(define-default-value 'loudness 86)

;;}}}
;;{{{ define-step-size

;;; define global step sizes
(define-step-size 'lax-breathiness 25)
(define-step-size 'average-pitch 20)
(define-step-size 'smoothness 15)
(define-step-size 'richness 25)
(define-step-size 'pitch-range 10)
(define-step-size 'breathiness 25)
(define-step-size 'head-size 10)
(define-step-size 'speech-rate 25)
(define-step-size 'left-volume 10)
(define-step-size 'right-volume 10)
(define-step-size 'laryngilization 10)
(define-step-size 'baseline-fall 10)
(define-step-size 'hat-rise 10)
(define-step-size 'stress-rise 10)
(define-step-size 'assertiveness 25)
(define-step-size 'quickness 20)
(define-step-size 'loudness 5)

;;}}}
;;{{{ define-unit-size

(define-unit-size 'head-size 'absolute)
(define-unit-size 'breathiness 'decibel)
(define-unit-size 'lax-breathiness 'percent)
(define-unit-size 'smoothness 'percent)
(define-unit-size 'richness 'percent)
(define-unit-size 'speech-rate 'words-per-minute)
(define-unit-size 'average-pitch 'hertz)
(define-unit-size 'pitch-range 'percent)
(define-unit-size 'left-volume 'decibel)
#+multivoice (define-unit-size 'right-volume 'decibel)
#+express  (define-unit-size 'right-volume 'duration)
(define-unit-size 'laryngilization 'percent)
(define-unit-size 'baseline-fall 'hertz)
(define-unit-size 'hat-rise 'hertz )
(define-unit-size 'stress-rise 'hertz)
(define-unit-size 'assertiveness 'percent)
(define-unit-size 'quickness 'percent)
(define-unit-size 'loudness 'decibel)

;;}}}
;;{{{ define minimum and maximum values

;;{{{ minimum values

(define-minimum-value 'head-size      65)
(define-minimum-value 'breathiness       0)
(define-minimum-value 'lax-breathiness      0)
(define-minimum-value 'smoothness       0)
(define-minimum-value 'richness      0)
(define-minimum-value 'laryngilization 0)
(define-minimum-value 'baseline-fall 0)
(define-minimum-value 'hat-rise 2)
(define-minimum-value 'stress-rise 1)
(define-minimum-value 'assertiveness 0)
(define-minimum-value 'quickness 0)
(define-minimum-value 'average-pitch 50)
(define-minimum-value 'pitch-range 0)
(define-minimum-value 'left-volume 0)
(define-minimum-value 'right-volume 0)
(define-minimum-value 'loudness 0)
(define-minimum-value 'speech-rate 120)
;;}}}
;;{{{ Maximum values.

(define-maximum-value  'head-size 145)
(define-maximum-value 'breathiness 72)
(define-maximum-value 'lax-breathiness 100)
(define-maximum-value 'smoothness 100)
(define-maximum-value 'richness 100)
(define-maximum-value 'laryngilization 100)
(define-maximum-value 'baseline-fall 40)
(define-maximum-value 'hat-rise 100)
(define-maximum-value 'stress-rise 100)
(define-maximum-value 'assertiveness 100)
(define-maximum-value 'quickness 100)
(define-maximum-value 'average-pitch 350)
(define-maximum-value 'pitch-range 250)
(define-maximum-value 'left-volume 100)
(define-maximum-value 'right-volume  100)
(define-maximum-value 'loudness 86)
(define-maximum-value 'speech-rate 550)
;;}}}

;;}}}

;;{{{ define scale factors for final scaling

(define-final-scale-factor 'voice 'undefined)

;;}}}

