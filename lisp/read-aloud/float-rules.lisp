;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :cl-user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Mon Oct 25 09:33:03 EDT 1993
 ;;; Defines some floating reading rules.
;;; Uses delay-until. Relies on after methods on read-aloud to force
;;; the delay-objects at the appropriate time.
;;;

  ;;; Variable: *FOOTNOTE-COUNTER                              Author: raman
  ;;; Created: Mon Oct 25 16:50:01 1993

(defvar *footnote-counter*  1 "Footnote counter")

  ;;; Function: FOOTNOTE-COUNTER                               Author: raman
  ;;; Created: Mon Oct 25 16:51:49 1993

(defun footnote-counter () 
  "Return current value of footnote counter. "
  *footnote-counter*)

  ;;; Function: RESET-FOOTNOTE-COUNTER                         Author: raman

  ;;; Function: INCREMENT-FOOTNOTE-COUNTER                     Author: raman
  ;;; Created: Mon Oct 25 16:52:34 1993

(defun increment-footnote-counter () 
  "Increment footnote counter"
  (incf *footnote-counter*))


  ;;; Function: DECREMENT-FOOTNOTE-COUNTER                     Author: raman
  ;;; Created: Mon Oct 25 16:54:55 1993

(defun decrement-footnote-counter () 
  "Decrement footnote counter. "
  (decf *footnote-counter*)
  )
  ;;; Created: Mon Oct 25 16:50:33 1993

(defun reset-footnote-counter () 
  "Rest footnote counter. "
  (setf *footnote-counter 1 )
  )


  ;;; Variable: *FOOTNOTE-CUE*                                 Author: raman
  ;;; Created: Mon Oct 25 16:50:59 1993

(defvar *footnote-cue*
   "footnote"
  "Footnote cue. ")

(define-reading-state 'footnote-mark
    #'(lambda(state)
        (afl:step-by afl:*current-speech-state*
                     'afl:average-pitch 2 )))


(def-reading-rule (footnote float)
    "Make footnotes float. "
  (let ((counter (footnote-counter )))
    (afl:new-block
     (with-reading-state (reading-state 'footnote-mark)
       (afl:tts-queue  (format nil " ~a " counter ))))
    (increment-footnote-counter)
    (delay-until 'paragraph
                 #'(lambda() (without-float (footnote)
                                            (decrement-footnote-counter)
                                            (afl:new-block
                                             (with-reading-state
                                                 (reading-state
                                                  'annotation-voice)
                                               (read-aloud
                                                (format nil
                                                        "footnote ~a,"
                                                        counter )))
                                             (read-aloud footnote ))))))
  )

(def-reading-rule (fraction float)
    "float fractions to the end of math. "
  (unless (find 'math *can-this-have-floats* )
    (push 'math *can-this-have-floats*))
  (read-aloud "fraction" )
  (delay-until  'math
                #'(lambda()
                    (without-float (fraction) 
                    (read-aloud fraction ))))
                )
