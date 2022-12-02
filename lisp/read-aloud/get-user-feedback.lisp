;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)

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
