;;;   -*-   Mode: LISP -*-    ;;;
 
 
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(in-package :afl)

;;; Define structs early to aid inlining during compilation.

;;; Structure: VOICE-CODE                                    Author: raman
;;; Created: Tue Aug 11 14:58:10 1992

(defstruct (voice-code (:type list)) voice code)


;;{{{ Structure: REFERENCE                                     Author: raman

;;; implements reference variables. Uses a mutable object structure
;;; with one field val. A method reference-value is written to make
;;; the interface clean. If passed any lisp object reference-value
;;; returns it, except for objects of type reference in which case the
;;; slot value is returned.

;;; Created: Fri Aug  7 11:27:54 1992
;;; A mutable object that implements reference variables

(defstruct reference val)

;;}}}
;;{{{ dimension

;;;  Structure: DIMENSION                                     Author: raman
;;; note: the dimension voice is in a sense redundant. The other
;;; dimensions that occur in the speech space are independent of one
;;; another, they together determine the voice.
;;; Structure:  dimension Author: raman
;;; Created: Fri Aug  7 10:25:35 1992
;;; A dimension in speech space.
(defstruct dimension name value step-size )

;;}}}
;;{{{ *speech-dimensions*

;;; Variable: *LIST-OF-SPEECH-DIMENSIONS*                           Author: raman
;;; Created: Sat Aug  8 15:35:18 1992

(defvar *speech-dimensions*
'(
  VOICE QUICKNESS ASSERTIVENESS STRESS-RISE HAT-RISE
  BASELINE-FALL LARYNGILIZATION RICHNESS SMOOTHNESS SPEECH-RATE
  HEAD-SIZE BREATHINESS PITCH-RANGE AVERAGE-PITCH LAX-BREATHINESS)  
  "list of dimension names in  the speech space. ")

;;; Function: SPEECH-DIMENSIONS                             Author: raman
;;; Created: Sat Oct  3 12:18:18 1992

(defun speech-dimensions ()
  "Return current list of dimensions"
  *speech-dimensions*)



(eval
 `(defstruct (point-in-speech-space :named (:type list))
    ,@ *speech-dimensions*))
;;}}}

