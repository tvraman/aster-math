;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

;;; Modified: Sun May 16 14:50:55 EDT 1993

;;;
;;; ilisp has a bug which forces read-line to behave weirdly:
;;;  If read-line is used inside a function for the first time, it
;;;  returns the empty string without waiting for user input. This
;;;  means that read-line has to be called twice.
;;; Introducing a variable *buggy-ilisp-read-line*
;;; If T then call read-line twice, discarding the initial empty
;;; string.

  ;;; Variable: *PROMPT-CUE*                                   Author: raman
  ;;; Created: Sat May  1 13:06:02 1993

(defparameter *prompt-cue*
  "prompt"
  "Sound to play when prompting. ")

  ;;; Parameter: *GET-LABEL-WAIT*                              Author: raman
  ;;; Created: Sat May  1 12:55:39 1993

(defparameter *get-label-wait* 0
  "Seconds to wait to see if user wants to enter a label")

  ;;; Function: GET-LABEL-FROM-USER                            Author: raman
  ;;; Created: Sat May  1 12:56:31 1993

(defun get-label-from-user ()
  "Get label for this object from user.
Wait for *get-label-wait* seconds before returning. "
  (unless (zerop *get-label-wait*)
    (when (y-or-n-p "Do you want to enter a new label. ")
      (afl:tts-queue "enter label. ")
      (format t "~% enter label.~% "))))
