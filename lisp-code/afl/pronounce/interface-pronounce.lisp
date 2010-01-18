;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Thu Mar 25 09:09:43 EST 1993
;;; External interface to pronunciation module.
;;; Pronunciation is another component of the audio formatter having
;;; state.
;;; Provide local-set-state  global-set-state methods and make afl
;;; blocks work with pronunciations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;; Method: LOCAL-SET-STATE                                  Author: raman
  ;;; Created: Thu Mar 25 09:19:37 1993

(defmethod local-set-state ((mode symbol))
  "Set pronunciation mode. "
  (set-pronunciation-mode mode)
  (setf (total-audio-state-pronounce *current-total-audio-state*) mode)
  )

(defmethod global-set-state((mode symbol ))
  "Set global pronunciation mode. "
  (setf *global-pronunciation-mode* mode )
  )




