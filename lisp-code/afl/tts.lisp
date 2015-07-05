;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Connecting AFL module to TTS.

(export '( 
          pause await-silence
          interrogative
          exclamation
          primary-stress
          secondary-stress
          exclamatory-stress
          with-surrounding-pause
          force-speech
          send-text
          speak-number-string
          subclause-boundary
          ))


(defun send-space ()
  "Send a space to TTS."
  (tts:queue " "))

(proclaim '(inline set-period-pause))
;;; DTK specific.
(defun set-period-pause (msec)
  (send-text  (format nil "[:pp ~a]" msec)))

(proclaim '(inline set-comma-pause))
;;; DTK Specific 
(defun set-comma-pause(msec)
  (send-text (format nil "[:cp ~a]" msec)))
(export 'set-period-pause)
(export 'set-comma-pause)











;;; Function: PAUSE                                          Author: raman
;;; Created: Thu Nov 26 12:03:17 1992

(defun pause (milliseconds) 
  "Pause for so many milliseconds"
  (assert
   (typep milliseconds 'fixnum) nil
   "Milliseconds = ~a, which is not a number" milliseconds)
  (when (> milliseconds 0) 
    (tts:pause  milliseconds)))

;;; Macro: WITH-SURROUNDING-PAUSE                            Author: raman
;;; Created: Thu Nov 26 12:32:58 1992

(defmacro with-surrounding-pause (pause-amount &body body) 
  "Execute body with surrounding pause specified by pause-amount"
  `(progn
     (tts:pause ,pause-amount)
     ,@body
     (tts:pause ,pause-amount)))

(defun force-speech ()
  (tts:force))
  




(defun send-text (text)
  (tts:queue text))


(defun speak-number-string (number-string)
  (tts:queue number-string))
;;; no-op
(defun await-silence () t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intonation



(export '(
          subclause-boundary audio-prompt 
          high-intonation
          low-intonation
          high-low-intonation
          comma-intonation
          period-intonation
          paragraph-begin
          ))
  ;;; Function: SUBCLAUSE-BOUNDARY                             Author: raman
  ;;; Created: Tue Dec 22 17:22:18 1992
(proclaim '(inline subclause-boundary))
(defun subclause-boundary () 
  "Insert a subclause boundary"
  (tts:queue " ") ;not implemented 
  )




  ;;; Function: HIGH-INTONATION                                Author: raman
  ;;; Created: Thu Dec 31 12:03:25 1992
(proclaim '(inline high-intonation))
(defun high-intonation () 
  "Generate H*"
  (tts:queue " ") ; not implemented 
  )


  ;;; Function: LOW-INTONATION                                 Author: raman
  ;;; Created: Thu Dec 31 12:04:09 1992
(proclaim'(inline low-intonation))
(defun low-intonation () 
  "Generate L*"
  (tts:queue " ") ; not implemented 
  )


  ;;; Function: HIGH-LOW-INTONATION                            Author: raman
  ;;; Created: Thu Dec 31 12:04:48 1992

(proclaim '(inline high-low-intonation))
(defun high-low-intonation () 
  "Generate Hl*"
  (tts:queue " ") ; not implemented 
  )


  ;;; Function: COMMA-INTONATION                               Author: raman
  ;;; Created: Thu Feb  4 16:42:12 1993
(proclaim '(inline comma-intonation ))
(defun comma-intonation () 
  "Generate a comma intonation"
  (tts:queue " ") ; not implemented 
  )
(proclaim '(inline period-intonation ))
(defun period-intonation () 
  "Generate a period intonation"
  (tts:queue " ") ; not implemented 
  )


  ;;; Function: PARAGRAPH-BEGIN                                Author: raman
  ;;; Created: Sun Jan  3 10:32:42 1993

(proclaim '(inline paragraph-begin))
(defun paragraph-begin  () 
  "Begin a paragraph"
  (tts:queue " ") ; not implemented 
  )

(proclaim '(inline exclamation))
(defun exclamation  () 
  "Send an exclamation. "
  (tts:queue " ") ; not implemented 
  )

(proclaim '(inline interrogative))
(defun interrogative  () 
  "Send an interrogative. "
  (tts:queue " ") ; not implemented 
  )

(proclaim '(inline primary-stress))
(defun primary-stress  () 
  "Send a  primary-stress. "
  (tts:queue " ") ; not implemented 
  )

(proclaim '(inline secondary-stress))
(defun secondary-stress  () 
  "Send a  secondary-stress. "
  (tts:queue " ") ; not implemented 
  )

(proclaim '(inline exclamatory-stress))
(defun exclamatory-stress  () 
  "Send an   exclamatory-stress. "
  (tts:queue " ") ; not implemented 
  )



  ;;; Function: PROMPT               Author: raman
  ;;; Created: Wed Oct  6 13:27:04 1993
(proclaim '(inline audio-prompt))
(defun audio-prompt(control-string &rest arguments)
  "Send a prompt to the speech device and return T. "
  (tts:speak (format nil control-string arguments ))
  t)

