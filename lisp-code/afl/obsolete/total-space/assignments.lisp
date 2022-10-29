;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Fri Mar 26 11:56:23 EST 1993
;;;  Assignments for total state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod local-set-state  ((state total-audio-state))
  "Set the total state"
  (local-set-state (total-audio-state-speech state ))
  (local-set-state (total-audio-state-sound  state))
  (local-set-state (total-audio-state-pronounce state ))
  )

(defmethod global-set-state  ((state total-audio-state))
  "Set the total state"
  (global-set-state (total-audio-state-speech state ))
  (global-set-state (total-audio-state-sound  state))
  (global-set-state (total-audio-state-pronounce state ))
  )
