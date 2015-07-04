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

(export '*stream*)


(export 'should-i-continue?)


(export 'should-i-stop?)

;;; Progressively removing redundant definitions:

;;; Function: PAUSE                                          Author: raman
;;; Created: Thu Nov 26 12:03:17 1992

(defun pause (milliseconds) 
  "Pause for so many milliseconds"
  (assert (typep milliseconds 'fixnum) nil
          "Milliseconds = ~a, which is not a number"
          milliseconds)
  (when (> milliseconds 0) 
    (format afl::*stream* " [_<~d>] "
            milliseconds))
  )

;;; Macro: WITH-SURROUNDING-PAUSE                            Author: raman
;;; Created: Thu Nov 26 12:32:58 1992

(defmacro with-surrounding-pause (pause-amount &body body) 
  "Execute body with surrounding pause specified by pause-amount"
  `(progn (afl::pause ,pause-amount)
    ,@body
    (afl::pause ,pause-amount))
  )


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
  (format afl::*stream* "[/]")
  )


  ;;; Function: LOW-INTONATION                                 Author: raman
  ;;; Created: Thu Dec 31 12:04:09 1992
(proclaim'(inline low-intonation))
(defun low-intonation () 
  "Generate L*"
  (format afl::*stream* "[\]")
  )


  ;;; Function: HIGH-LOW-INTONATION                            Author: raman
  ;;; Created: Thu Dec 31 12:04:48 1992

(proclaim '(inline high-low-intonation))
(defun high-low-intonation () 
  "Generate Hl*"
  (format afl::*stream* "[/\]")
  )


  ;;; Function: COMMA-INTONATION                               Author: raman
  ;;; Created: Thu Feb  4 16:42:12 1993
(proclaim '(inline comma-intonation ))
(defun comma-intonation () 
  "Generate a comma intonation"
  (format *stream* "[_,] ")
  )
(proclaim '(inline period-intonation ))
(defun period-intonation () 
  "Generate a period intonation"
  (format *stream* "[_.] ")
  )


  ;;; Function: PARAGRAPH-BEGIN                                Author: raman
  ;;; Created: Sun Jan  3 10:32:42 1993

(proclaim '(inline paragraph-begin))
(defun paragraph-begin  () 
  "Begin a paragraph"
  (format *stream* "[+]")
  )

(proclaim '(inline exclamation))
(defun exclamation  () 
  "Send an exclamation. "
  (format *stream* "[_!]")
  )

(proclaim '(inline interrogative))
(defun interrogative  () 
  "Send an interrogative. "
  (format *stream* "[_?]")
  )

(proclaim '(inline primary-stress))
(defun primary-stress  () 
  "Send a  primary-stress. "
  (format *stream* "[']")
  )

(proclaim '(inline secondary-stress))
(defun secondary-stress  () 
  "Send a  secondary-stress. "
  (format *stream* "[`]")
  )

(proclaim '(inline exclamatory-stress))
(defun exclamatory-stress  () 
  "Send an   exclamatory-stress. "
  (format *stream* "[\"]")
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

