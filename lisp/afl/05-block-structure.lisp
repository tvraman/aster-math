;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)

(export '(
          *current-speech-state* *global-speech-state*
                                        ;*await-silence-when-using-stereo*
          local-set-state global-set-state
          new-block exit-block))
;;;  Contains definition of new-block as a macro.
;;; and associated assignment operators.

;;{{{ *current-speech-state*

;;; Variable: *GLOBAL-SPEECH-STATE*                                 Author: raman
;;; Created: Mon Sep  7 09:02:07 1992

(defvar *global-speech-state* nil "records global state of afl")
;;; Variable: *CURRENT-SPEECH-STATE*                                Author: raman
;;; Created: Fri Aug 14 10:29:11 1992

(defvar *current-speech-state* nil "Records current state of the audio formatter ")

;;; <(*modified-dimensions* no longer used. )>

;;}}}
;;{{{ new-block

;;; Modified: Wed Feb 10 15:40:09 EST 1993
;;; No longer using *modified-dimensions* this taken care of by
;;; set-speech-state.
;;; <(backed up old version with comments. )>
;;; relies on set-speech-state.
;;; Uses dynamic variable *current-speech-state* which records the speech
;;; state.
;;; Modified: Tue Mar 23 10:42:11 EST 1993
;;; Phasing out named-block. new-block now allows exits.
;;; eventually should be renamed to afl-block.

;;; Modified: Wed Feb 10 15:44:59 EST 1993
;;; <(Modifying to stop using *modified-dimensions*)>
;;; Macro: NAMED-BLOCK                                       Author: raman
;;; Created: Thu Aug 27 19:12:29 1992

;;; all blocks should have exits.
;;; generate block name automatically.
;;; eventually do away with named-block
  
(defun afl-block-name()
  "Generate a afl block name"
  (gensym "afl-block-"))

;;; Modified: Tue Mar 23 13:11:56 EST 1993
;;; new block now handles speech and sound components
;;; Modified: Thu Mar 25 09:22:34 EST 1993
;;; Blocks now handle pronunciation component as well
;;; Modified: Fri Mar 26 11:15:12 EST 1993
;;; Introducing total audio state
;;; which is the direct sum of the component states

(defmacro new-block (   &body body)
  "sets up a new block in afl"
  (let ((name (afl-block-name )))
    `(block
         ,name
       (let* ((previous-speech-state *current-speech-state*)
              (*current-speech-state* (copy-point-in-speech-space *current-speech-state* ))
              (previous-pronunciation-mode *pronunciation-mode*))
         (unwind-protect
              (progn ,@body)
;;; reset state
           (progn
             (set-speech-state previous-speech-state )
             (set-pronunciation-mode previous-pronunciation-mode)))))))

;;; Function: EXIT-BLOCK                                     Author: raman
;;; Created: Fri Aug 28 08:55:43 1992

;;}}}
;;{{{ assignments

;;; Modified: Thu Aug 20 11:00:46 EDT 1992
;;; accept additional keyword argument changed-dimensions whose
;;; default value is *speech-dimensions*
;;; Modified: Mon Aug 24 08:21:06 EDT 1992

;;; Modified: Sat Oct  3 14:14:22 EDT 1992
;;; surround  body of set-speech-state in with-interruptions-inhibited so that
;;; an interrupt will not leave the multivoice in a weird state. Note:
;;; If this is not done, there is a chance of a user interrupt
;;; arriving when the multivoice has received part of a control
;;; string, leaving it in phoenetic mode.
;;; Modified: Sat Oct 17 11:54:10 EDT 1992








;;; Function: SET-speech-STATE                                      Author: raman
;;; Created: Mon Aug 10 13:43:53 1992

;;; <(old function  set-state backed up. )>

;;; Wed Feb 10 11:30:56 EST 1993
;;; since it is set-speech-state that needs to know about changed dimensions,
;;; it is cleaner to make it compute the changed dimensions rather
;;; than passing it around.
;;; Writing a new version that keeps track of the hardware state,
;;; checks this against the new state to compute changed-dimensions,
;;; and then sets the changed dimensions.

  ;;; Variable: *SPEECH-HARDWARE-STATE*                        Author: raman
  ;;; Created: Wed Feb 10 11:34:41 1993

(defvar *speech-hardware-state* nil "Holds current state of speech
hardware")
(defun set-speech-state (state )
  "sets state of audio  formatter to state   after applying final
scaling"
  (assert (point-in-speech-space-p state) nil
          "~a is not a point in speech space" state)
  (let*
      ((new-state (scale-point-in-speech-space state ))
       (modified-dimensions
         (compute-modified-dimensions
          *speech-hardware-state* new-state  ))
       (command-string nil))
    (when modified-dimensions
      (dolist
          (dim-name (remove-duplicates modified-dimensions))
        (let ((dimension (point-accessor dim-name new-state )))
          (setf
           command-string
           (concatenate
            'string
            command-string
            (generate-synthesizer-command
             (dimension-name dimension)
             (reference-value (dimension-value dimension )))))))
      (setf *speech-hardware-state* new-state )
      (tts-code  command-string))))

  ;;; Function: COMPUTE-MODIFIED-DIMENSIONS                    Author: raman
  ;;; Created: Wed Feb 10 11:47:34 1993
;;; Relies on representation of point-in-speech-space
;;; forward declaration: see 01-speech-space for real definition

(defun compute-modified-dimensions (old-point new-point )
  "Return names of dimensions that are changed in new-point"
  (cond
    ((null old-point ) (speech-dimensions))
    (t (assert  (and
                 (point-in-speech-space-p  old-point)
                 (point-in-speech-space-p new-point )) nil
                 "Arguments are not valid points in speech space. ")
       (let ((modified-dimensions nil ))
         (dolist
             (dim-name (speech-dimensions))
           (unless (same-dimension-value
                    (point-accessor  dim-name old-point)
                    (point-accessor dim-name new-point ))
             (push dim-name modified-dimensions  )))
         (if (find 'voice modified-dimensions )
             (speech-dimensions)
             modified-dimensions)))))

  ;;; Function: SAME-DIMENSION-VALUE                           Author: raman
  ;;; Created: Wed Feb 10 11:56:58 1993

(defun same-dimension-value (dimension-1 dimension-2)
  "Do these have the same value?"
  (equal
   (reference-value (dimension-value dimension-1 ))
   (reference-value (dimension-value dimension-2  )))
  )



;;; Wed Feb 10 12:11:45 EST 1993
;;; uses set-speech-state.
;;; METHOD: local-set-state
;;; This method allows the user to define methods for local-set-state
;;; for specific spaces. The following method is for the speech space.
;;;

(defmethod     local-set-state  ( new-state )
  "Set current speech state of afl to new-state"
  (assert (point-in-speech-space-p new-state ) nil
          "~a is not a point in speech space"
          new-state )
  (setf *current-speech-state*   new-state)
  (set-speech-state   *current-speech-state* ))

;;; <(global-set! no longer used. )>

;;; METHOD: GLOBAL-SET-STATE                               Author: raman
;;; Created: Fri Sep  4 15:45:24 1992
;;; Modified: Wed Feb 10 15:33:48 EST 1993
;;; uses set-speech-state now.
;;; Has been changed to a method. was an ordinary function before
(defmethod  global-set-state (new-state )
  "set global speech state of afl"
  (assert (point-in-speech-space-p new-state) nil
          "~a is not a point in speech space"
          new-state )
  (dolist
      (dimension (speech-dimensions))
    (set-global-value dimension
                      (current-value dimension new-state))
    (set-step-size dimension
                   (current-step-size dimension new-state))
    )
  (set-speech-state  *current-speech-state*)

  )

;;; use this for setting single global value
;;; Modified: Sat Oct 17 12:29:19 EDT 1992
;;; no longer needed or used, throw away.

;;; Function: GLOBAL-SET-VALUE                               Author: raman
;;; Created: Sat Sep  5 10:11:51 1992

(defun global-set-value (dimension value)
  "set value globally along dimension"
  (assert (find dimension (speech-dimensions)) nil
          "~a is not a valid dimension"
          dimension)
  (setf
   (reference-val (gethash dimension (global-values)))
   value)

  (set-speech-state  *current-speech-state* )

                                        ; actually send the commands.
  )

;;}}}
