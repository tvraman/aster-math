;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;

(in-package :afl)

;;; Initially assumes dectalk, but to be flexible enough to allow any
;;; synthesizer.

;;{{{ generate-synthesizer-command

;;; Function: GENERATE-SYNTHESIZER-COMMAND                   Author: raman
;;; Created: Tue Aug 11 13:41:18 1992

(defun generate-synthesizer-command (dimension value)
  "Generates synthesizer command that sets dimension dimension to
  value value"
  (flet (
         (bound-in-interval(dim-name   value)
           "fix value inside permitted interval"
           (min
            (max (minimum-value dim-name) value)
            (maximum-value dim-name ))))
    (when (equal dimension 'voice)
      (setf value (get-voice-code value)))
    (concatenate
     'string
     "["
     (get-synthesizer-code dimension)
     (if (stringp value)
         value
         (prin1-to-string (bound-in-interval dimension (round value))))
     "]")))

;;}}}
;;{{{ *voice-codes-table*

;;; Variable: *VOICE-CODES-TABLE*                            Author: raman
;;; Created: Tue Aug 11 14:56:19 1992

(defvar *voice-codes-table* nil "table of voice codes for dectalk")

;;; Function:  DEFINE-VOICE-CODE Author: raman Created: Tue Aug 11 14:56:56 1992

(defun define-voice-code (voice code)
  "define code as code for voice voice"
  (push
   (make-voice-code :voice  voice :code code)
   *voice-codes-table*))

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
  (let ((voice-code
          (find voice *voice-codes-table* :key #'voice-code-voice )))
    (if  voice-code
         (voice-code-code voice-code)
         (error "Voice code for voice ~a not defined" voice))))

;;}}}
