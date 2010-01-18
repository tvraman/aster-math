;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))



;;; Initially assumes dectalk, but to be flexible enough to allow any
;;; synthesizer.

;;; { Start and end markers 

;;; Variable: *BEGIN-COMMAND*                                Author: raman
;;; Created: Tue Aug 11 13:40:16 1992
;;; external variable: 
(defvar *begin-command* "[" " string that begins a synthesizer command")

;;; Variable: *END-COMMAND*                                  Author: raman
;;; Created: Tue Aug 11 13:40:44 1992
;;; external variable: 
(defvar *end-command* "]" "string that ends a synthesizer command")

;;; }
;;; { generate-synthesizer-command
;;; Modified: Thu Aug 20 10:40:53 EDT 1992
;;; Modified to return string rather than call format each time.
;;; Function: GENERATE-SYNTHESIZER-COMMAND                   Author: raman
;;; Created: Tue Aug 11 13:41:18 1992

(defun generate-synthesizer-command (dimension value) 
  "Generates synthesizer command that sets dimension dimension to
  value value"
  (flet (
         (bound-in-interval(dim-name   value)
           "fix value inside permitted interval"
;           (format t "~& dim-name = ~a
;~& value = ~a"
;                   dim-name value)
           (min
            (max (minimum-value dim-name)
                 value)
            (maximum-value dim-name ))))
    (when (equal dimension 'voice)
      (setf value (get-voice-code value)))
    (concatenate 'string
                 *begin-command*
                 " "
                 (get-synthesizer-code dimension)
                 (if (stringp value)
                     value
                     (prin1-to-string
                      (bound-in-interval dimension 
                                         (round value))))
                 " "
                 *end-command*))
  )

;;; }

;;; { *voice-codes-table*

;;; Variable: *VOICE-CODES-TABLE*                            Author: raman
;;; Created: Tue Aug 11 14:56:19 1992

(defvar *voice-codes-table* nil "table of voice codes for dectalk")

;;; Structure: VOICE-CODE                                    Author: raman
;;; Created: Tue Aug 11 14:58:10 1992

(defstruct (voice-code
             (:named)
             (:type list))
  voice
  code)
;;; Modified: Thu Aug 20 09:37:22 EDT 1992
;;; export voice whose code is defined
;;; Function:  DEFINE-VOICE-CODE Author: raman Created: Tue Aug 11 14:56:56 1992

(defun define-voice-code (voice code) 
  "define code as code for voice voice"
  (export (list voice))
  (push
   (make-voice-code :voice  voice
                    :code code)
   *voice-codes-table*
   )
  )

(define-voice-code 'paul "p")
(define-voice-code 'harry "h")
(define-voice-code 'dennis "d")
(define-voice-code 'frank "f")
(define-voice-code 'betty "b")
(define-voice-code 'wendy "w")
(define-voice-code  'rita  "r") 
(define-voice-code 'ursula "u")
(define-voice-code 'kid "k")

;;; Function: GET-VOICE-CODE                                 Author: raman
;;; Created: Tue Aug 11 15:03:42 1992

(defun get-voice-code (voice) 
  "get code for voice"
  (let
      ((voice-code
        (find voice *voice-codes-table* :key #'voice-code-voice )))
    (if  voice-code
         (voice-code-code voice-code)
         (error "Voice code for voice ~a not defined"
                voice)
         )
    )
  )

;;; }

