;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Fri Mar 26 11:21:40 EST 1993;;; Initialize total audio space.
   ;;; Function: INITIALIZE-TOTAL-SPACE                         Author: raman
  ;;; Created: Fri Mar 26 11:18:46 1993

(export '(initialize-total-space ))
(defun initialize-total-space () 
  "Initialize total audio space"
  (setf *current-total-audio-state*
        (make-total-audio-state
         :speech *current-speech-state*
         :sound *current-audio-state*
         :pronounce *pronunciation-mode* ))
  (setf *global-total-audio-state* *current-total-audio-state* )
  )
