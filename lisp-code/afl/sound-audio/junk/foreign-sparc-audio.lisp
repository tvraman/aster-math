;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package 'user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Sun Jan 10 12:34:15 EST 1993
;;; Sparc audio stuff using C program and the lcl:def-foreign-function
;;; interface.

;;; Setting play port
(defvar *switch-port-pathname*
  "/usr/u/raman/lisp-code/afl/sound-audio/switch_play_port.o"
  "C function to switch play port ")

(lcl:load-foreign-files  *switch-port-pathname*)


(lcl:def-foreign-function (switch-audio-port-internal
                           (:name "_set_port")
                           (:return-type :signed-32bit))
    (port :signed-32bit)
  )


  ;;; Function: SWITCH-AUDIO-PORT                              Author: raman
  ;;; Created: Sun Jan 10 12:45:05 1993

(defun switch-audio-port (port) 
  "Switch audio port either :headphone or :speaker"
  (cond
    ((eql :headphone port ) (switch-audio-port-internal 1))
    ((eql :h port ) (switch-audio-port-internal 1))
    ((eql :speaker port) (switch-audio-port-internal 0))
    ((eql :s port) (switch-audio-port-internal 0))
    (t (error "unknown play port ~a " port ))
    )
  )

