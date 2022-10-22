;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :dectalk)
;#+lucid (use-package :lucid-common-lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to implement smart talking.
;;; Directional audio, smart stopping during speech etc.
;;; Created: Fri Apr  3 08:36:01 EST 1992
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(export '(
          speak-number-string 
          get-user-key-press
          send-space 
          force-speech 
	  should-i-continue?
          should-i-stop?
	  send-text
          speak-text  speak-file 
	  ))


;;; Variable: *PUNCTUATIONS*                                 Author: raman
;;; Created: Sat Apr 11 19:55:50 1992

(defvar *punctuations* nil "list of known punctuation characters.")

(setf *punctuations*
      (list
       "."
       ","
       "!"
       "?"
       ";" 
       ))

;;; Function: PUNCTUATION?                                   Author: raman
;;; Created: Sat Apr 11 19:54:58 1992

(defun punctuation? (punct) 
  "check if argument is a punctuation character"
  (and (stringp punct)
       (find punct *punctuations*
	     :test #'string=))
  )



;;; Function: SEND-TEXT                                      Author: raman
;;; Created: Sat Apr 11 20:47:33 1992

(proclaim '(inline send-text)) 
(defun send-text (text ) 
  "send text to stream connected to dectalk"
  (format *stream*  "~a" text)
  )

(defun speak-file (filename) 
  "Speak the contents of the file. "
  (with-open-file  
   (stream filename :direction :input )
   (send-text (read stream ))))

;;; Variable: *END-OF-SENTENCE-MARKER*                       Author: raman
;;; Created: Mon Apr 13 11:16:00 1992

(defvar *end-of-sentence-marker* nil "end of sentence markers")

(setf *end-of-sentence-marker*
      (list
       "."
       "?"
       "!"
       ))


;;; Function: END-OF-SENTENCE?                               Author: raman
;;; Created: Mon Apr 13 11:16:46 1992

(defun end-of-sentence? (text) 
  "check if texts marks the  end of a sentence. "
  
  (find text *end-of-sentence-marker*
	:test #'string=)
  )


;;; Function: SHOULD-I-CONTINUE?                             Author: raman
;;; Created: Sat Apr 11 21:22:18 1992

(proclaim '(inline should-i-continue?))
(defun should-i-continue? () 
  "check if user wants me to shut up!"
  (cond
    (*dribbling-to-file* t) 
  (t (await-silence)
  (not (eq #\s
           (read-char-no-hang ))))
  )
  )


;;; Function: SHOULD-I-STOP?                                 Author: raman
;;; Created: Thu Apr 16 09:18:47 1992

(proclaim '(inline should-i-stop?)) 
(defun should-i-stop? () 
  "check if I should stop talking."
  (unless *dribbling-to-file*
  (await-silence)
  (eq #\s
      (read-char-no-hang))
  )
  )



  ;;; Function: GET-USER-KEY-PRESS                             Author: raman
  ;;; Created: Wed Jan  6 11:26:54 1993
(proclaim '(inline get-user-key-press ))
(defun get-user-key-press () 
  "Get the next key press from user"
  (await-silence)
  (read-char-no-hang)
  )

;;; Function: SPEAK                                          Author: raman
;;; Created: Fri Sep 25 09:25:58 1992

(proclaim '(inline  speak-text)) 
(defun speak (text) 
  "speak text on multivoice"
(format *stream* "~a" text)
)



;;; Function: FORCE-SPEECH                                   Author: raman
;;; Created: Sat Sep 26 13:23:50 1992
(proclaim '(inline force-speech))
(defun force-speech () 
  "force dectalk to speak"
  (write-char #\VT *stream*)            ; clause terminator
  (write-char #\Return *stream*)        ;send return and force speech
  (await-silence)
  )

;;; Function: SPEAK-NUMBER-STRING                            Author: raman
;;; Created: Sat Sep 26 17:57:11 1992
#+clisp 
(defun speak-number-string (number-string) 
  "speak number string"
  (let ((number-string-stream (make-string-output-stream )))
    (format number-string-stream " ")
    (loop for char across number-string
          when (equal char #\.) 
          do  (format number-string-stream  " point ")
          else  do  (format number-string-stream  "~a" char))
    (format *stream*
            (get-output-stream-string number-string-stream ))
    )
  )

#+lucid
(defun speak-number-string (number-string) 
  "speak number string"
  (let ((number-string-stream (make-string-output-stream )))
    (format number-string-stream " ")
    (loop for char being the array-element of  number-string
          when (equal char #\.) 
          do  (format number-string-stream  " point ")
          else  do  (format number-string-stream  "~a" char))
    (format *stream*
            (get-output-stream-string number-string-stream ))
    )
  )

;;; Function: SPACE                                          Author: raman
;;; Created: Tue Nov 24 11:52:52 1992

(defun send-space () 
  "Send a space to the dectalk"
  (format *stream* " ")) 

