;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;; $Id: point-in-audio-space.lisp,v 1.1.1.1 2001/08/10 23:20:19 raman Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Sat Feb 20 16:19:43 EST 1993
;;; Contains macro to define points in audio space.
;;; dimensions are held in a global variable *audio-dimensions*
;;; Uses system:structure-ref which is lucid specific.
;;; clisp note:
;;; clisp equivalent is system::%structure-ref.
;;; Differences:
;;; Order of args is different.
;;; Under lucid:  args == instance index type
;;; index is 0 based.
;;; Under clisp:
;;; args = type instance index
;;; index is 1-based, 0 returns type
;;; Traded off against using the list representation which was used
;;; for point-in-speech-space
;;; <(Code used to learn this use in scratch directory)>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; First define global values:
;;; { global audio values

  ;;; Variable: *AUDIO-GLOBALS*                                Author: raman
  ;;; Created: Thu Feb 11 20:07:01 1993

(defvar *audio-globals*
  (make-hash-table :test #'equal )
  "Global audio values")

  ;;; Function: DEFINE-AUDIO-GLOBAL-VALUE                      Author: raman
  ;;; Created: Thu Feb 11 20:08:26 1993

(defun define-audio-global-value (dimension value)
  "Define global audio value for this dimension"
  (setf (gethash dimension *audio-globals*)
        (make-reference :val value ))
  )
(defvar *default-sound-filename*
  (concatenate 'string
               user::*lisp-code-directory*
               "/"
               "sounds/soft-beep.au" )
  "Default sound")
  ;;; Function: GET-AUDIO-GLOBAL-VALUE                         Author: raman
  ;;; Created: Thu Feb 11 20:09:49 1993

(defun get-audio-global-value (dimension) 
  "Get global audio value"
  (gethash dimension *audio-globals*)
  )

(define-audio-global-value 'id nil )
(define-audio-global-value 'switch nil)
(define-audio-global-value 'sound-name  *default-sound-filename*)
(define-audio-global-value'volume 1)
(define-audio-global-value 'port :h)
(define-audio-global-value 'interval 0)

  ;;; Function: SET-AUDIO-GLOBAL-VALUE                         Author: raman
  ;;; Created: Thu Feb 11 20:22:49 1993

(defun set-audio-global-value (dimension value) 
  "Set global value for this audio dimension"
  (setf (reference-val (gethash dimension *audio-globals*) )
        value)
  )
;;; }

  ;;; Variable: *AUDIO-DIMENSIONS*                             Author: raman
  ;;; Created: Sat Feb 20 16:21:48 1993

(defvar *audio-dimensions*
  (list  'switch  'interval 'port 'volume 'sound-name 'id )
  "List of dimensions in audio space")

;;; first make symbols present so find-symbol can find them in the
;;; following macro.

(loop for slot in *audio-dimensions* do
      (intern (format nil "POINT-IN-AUDIO-SPACE-~a" slot)
              #.(package-name *package* ))
      )
  ;;; Macro: DEFINE-POINT-IN-AUDIO-SPACE                       Author: raman
  ;;; Created: Sat Feb 20 16:24:20 1993

(defmacro define-point-in-audio-space () 
  "Defines points in audio space. "
  `(progn
    (defstruct (point-in-audio-space )
      ,@(loop for dim in *audio-dimensions* collect 
              `(,dim (get-audio-global-value  ',dim )))
      )
                                        ; define accessor
    (defun audio-point-accessor (struct slot-name )
      (ecase  slot-name
        .,
        (loop for slot in *audio-dimensions*
              collect 
              `(,slot (,(find-symbol
                         (format nil "POINT-IN-AUDIO-SPACE-~a" slot)
                         #.(package-name *package* )) struct )))))
    
  ;;; generate defsetf forms for the accessor
    #+LUCID 
    (loop for slot in *audio-dimensions* do 
     (defsetf audio-point-accessor (struct slot) (value) 
       `(setf  (system:structure-ref ,struct 
                (position ,slot *audio-dimensions*)
                'point-in-audio-space) ,value ))))
  )

#+clisp
(loop for slot in *audio-dimensions* do 
     (defsetf audio-point-accessor (struct slot) (value) 
       `(setf  (system::%structure-ref 'point-in-audio-space
                ,struct
                (+ 1 (position ,slot *audio-dimensions* )))  ,value )))
