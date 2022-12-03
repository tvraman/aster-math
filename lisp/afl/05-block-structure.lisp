;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)

(export
 '(*current-speech-state* *global-speech-state*
   local-set-state new-block))

;;;  Contains definition of new-block as a macro.
;;; and associated assignment operators.

;;{{{ *current-speech-state*

;;; Variable: *GLOBAL-SPEECH-STATE*                                 Author: raman
;;; Created: Mon Sep  7 09:02:07 1992

(defvar *global-speech-state* nil "records global state of afl")

;;; Variable: *CURRENT-SPEECH-STATE*                                Author: raman
;;; Created: Fri Aug 14 10:29:11 1992

(defvar *current-speech-state* nil "Records current state of the audio formatter ")

;;}}}
;;{{{ new-block

;;; Macro: NEW-BLOCK                                       Author: raman
;;; Created: Thu Aug 27 19:12:29 1992
;;; generate block name automatically.

(defun afl-block-name()
  "Generate a afl block name"
  (gensym "afl-block-"))

(defmacro new-block (   &body body)
  "sets up a new block in afl"
  (let ((name (afl-block-name )))
    `(block ,name
       (let* ((previous-speech-state *current-speech-state*)
              (*current-speech-state* (copy-point-in-speech-space *current-speech-state* ))
              (previous-pronunciation-mode *pronunciation-mode*))
         (unwind-protect
              (progn ,@body)
;;; reset state
           (progn
             (set-speech-state previous-speech-state )
             (set-pronunciation-mode previous-pronunciation-mode)))))))

;;}}}
;;{{{ assignments

  ;;; Variable: *SPEECH-HARDWARE-STATE*                        Author: raman
  ;;; Created: Wed Feb 10 11:34:41 1993

(defvar *speech-hardware-state* nil
  "Holds current state of speech hardware")

(defun set-speech-state (state )
  "sets state of audio  formatter to state   after applying final scaling"
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

(defun compute-modified-dimensions (old-point new-point )
  "Return names of dimensions that are changed in new-point"
  (cond
    ((null old-point ) (speech-dimensions))
    (t
     (assert
      (and
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
   (reference-value (dimension-value dimension-2  ))))

;;; Wed Feb 10 12:11:45 EST 1993
;;; uses set-speech-state.
;;; METHOD: local-set-state

(defmethod     local-set-state  ( new-state )
  "Set current speech state of afl to new-state"
  (assert (point-in-speech-space-p new-state ) nil
          "~a is not a point in speech space" new-state )
  (setf *current-speech-state*   new-state)
  (set-speech-state   *current-speech-state* ))

;;}}}
