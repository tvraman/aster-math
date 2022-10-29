;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Fri Mar 26 11:11:14 EST 1993
;;; Structures and functions to help record the total state of the
;;; audio space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*current-total-audio-state* *global-total-audio-state*))
  ;;; Structure: TOTAL-AUDIO-STATE                               Author: raman
  ;;; Created: Fri Mar 26 11:11:37 1993
;(eval-when (compile load eval ) 
  (defstruct total-audio-state
    (speech nil)
    (sound nil)
    (pronounce nil)
    )
;  )


  ;;; Variable: *CURRENT-TOTAL-AUDIO-STATE*                      Author: raman
  ;;; Created: Fri Mar 26 11:12:56 1993

(defvar *current-total-audio-state* nil
  "The total state of audio formatter in current scope")


  ;;; Variable: *GLOBAL-TOTAL-AUDIO-STATE*                       Author: raman
  ;;; Created: Fri Mar 26 11:13:44 1993

(defvar *global-total-audio-state* nil
  "The total state of the audio formatter in the global scope")




