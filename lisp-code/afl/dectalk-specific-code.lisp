;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Redefining some of the more important functions in the afl
;;; package.


(export '( 
          pause
          interrogative
          exclamation
          primary-stress
          secondary-stress
          exclamatory-stress
          reset-dectalk
          with-surrounding-pause
          force-speech
          send-text
          speak-number-string
          subclause-boundary
          ))


(export 'send-space)

(export 'speak-file)

(export (list 'with-file-as-well-as-dectalk
              'with-file-instead-of-dectalk ))
(proclaim '(inline set-period-pause))
(defun set-period-pause (msec)
  (send-text  (format nil "[:pp ~a]" msec)
              ))
(proclaim '(inline set-comma-pause))
(defun set-comma-pause(msec)
  (send-text
   (format nil "[:cp ~a]" msec))
  )
(export 'set-period-pause)
(export 'set-comma-pause)
(export 'should-i-continue?)


(export 'should-i-stop?)

;;; Progressively removing redundant definitions:






;;; Macro: WITH-SURROUNDING-PAUSE                            Author: raman
;;; Created: Thu Nov 26 12:32:58 1992

(export 'force-speech)

(export 'synchronize-and-play)


(export 'send-text)


(export 'speak-number-string )

(export 'await-silence)
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
  (format t "[)]")
  )




  ;;; Function: HIGH-INTONATION                                Author: raman
  ;;; Created: Thu Dec 31 12:03:25 1992
(proclaim '(inline high-intonation))
(defun high-intonation () 
  "Generate H*"
  (tts:queue  "[/]"))


  ;;; Function: LOW-INTONATION                                 Author: raman
  ;;; Created: Thu Dec 31 12:04:09 1992
(proclaim'(inline low-intonation))
(defun low-intonation () 
  "Generate L*"
  (tts:queue "[\]")
  )


  ;;; Function: HIGH-LOW-INTONATION                            Author: raman
  ;;; Created: Thu Dec 31 12:04:48 1992

(proclaim '(inline high-low-intonation))
(defun high-low-intonation () 
  "Generate Hl*"
  (tts:queue "[/\]")
  )


  ;;; Function: COMMA-INTONATION                               Author: raman
  ;;; Created: Thu Feb  4 16:42:12 1993
(proclaim '(inline comma-intonation ))
(defun comma-intonation () 
  "Generate a comma intonation"
  (tts:queue "[_,] ")
  )
(proclaim '(inline period-intonation ))
(defun period-intonation () 
  "Generate a period intonation"
  (tts:queue "[_.] ")
  )


  ;;; Function: PARAGRAPH-BEGIN                                Author: raman
  ;;; Created: Sun Jan  3 10:32:42 1993

(proclaim '(inline paragraph-begin))
(defun paragraph-begin  () 
  "Begin a paragraph"
  (tts:queue "[+]")
  )

(proclaim '(inline exclamation))
(defun exclamation  () 
  "Send an exclamation. "
  (tts:queue "[_!]")
  )

(proclaim '(inline interrogative))
(defun interrogative  () 
  "Send an interrogative. "
  (tts:queue "[_?]")
  )

(proclaim '(inline primary-stress))
(defun primary-stress  () 
  "Send a  primary-stress. "
  (tts:queue "[']")
  )

(proclaim '(inline secondary-stress))
(defun secondary-stress  () 
  "Send a  secondary-stress. "
  (tts:queue "[`]")
  )

(proclaim '(inline exclamatory-stress))
(defun exclamatory-stress  () 
  "Send an   exclamatory-stress. "
  (tts:queue "[\"]")
  )


(defun reset-dectalk()
  "reset dectalk, use after powering up dectalk. "
  (with-lazy-set-state
      (set-speech-state *current-speech-state*))
  )
  ;;; Function: PROMPT               Author: raman
  ;;; Created: Wed Oct  6 13:27:04 1993
(proclaim '(inline audio-prompt))
(defun audio-prompt(control-string &rest arguments)
  "Send a prompt to the speech device and return T. "
  (afl::send-text
   (format nil control-string arguments ))
  t)

