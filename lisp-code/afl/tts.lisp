;;;   -*- Syntax: Common-Lisp; Mode: LISP -*-    ;;;
;;; tts.lisp -- Common Lisp interface to Emacspeak speech server
;;; $Author: tv.raman.tv $
;;; Description: Interface Common Lisp to Emacspeak TTS servers
;;; Keywords: AsTeR, Emacspeak, Audio Desktop
;;{{{ Copyright:

;;; Copyright (C) 2011 -- 2016, T. V. Raman<tv.raman.tv@gmail.com>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;;; Interface Common Lisp to Emacspeak TTS servers

;;}}}
;;{{{ Setup:
(in-package :afl)

(export
 '(tts-init tts-open tts-shutdown
   tts-icon tts-speak tts-force tts-queue  tts-say tts-pause
   with-surrounding-pause
   high-intonation low-intonation high-low-intonation
   comma-intonation period-intonation
   set-period-pause set-comma-pause
   interrogative exclamation
   primary-stress secondary-stress exclamatory-stress
   subclause-boundary))
;;; A TTS structure holds the engine name, process handle, and input/output streams.
(defstruct tts engine process input output )

(defvar *emacspeak* "/home/raman/emacs/lisp/emacspeak/"
  "Root of Emacspeak installation.")
(defun tts-location (engine)
  "Return location of specified engine."
  (declare (special *emacspeak*))
  (concatenate 'string *emacspeak* "servers/" engine))

(defun tts-dtk ()
  "Return name of dtk-soft server."
  (declare (special *emacspeak*))
  (concatenate 'string *emacspeak* "servers/dtk-soft"))

(defun tts-outloud ()
  "Outloud tcl server"
  (declare (special *emacspeak*))
  (concatenate 'string *emacspeak* "servers/outloud"))

(defvar *tts* nil
  "Handle to tts server connection.")

(defun tts-init (&key (engine "dtk-soft"))
  "Initialize TTS  system."
  (declare (special *tts*))
  (setq *tts*
        (make-tts :engine (tts-location engine)))
  (tts-open))


(defun tts ()
  "Return handle to TTS server."
  (declare (special *tts*))
  *tts*)

;;}}}
;;{{{macros:

(defmacro with-surrounding-pause (pause-amount &body body)
  "Execute body with surrounding pause specified by pause-amount"
  `(progn
     (afl:tts-pause ,pause-amount)
     ,@body
     (afl:tts-pause ,pause-amount)))
;;}}}
;;{{{Internal Functions

(defun tts-open ()
  "Open a TTS session."
  (let ((handle (tts)))
    (setf (tts-process handle)
          (sb-ext:run-program
           (tts-engine handle) nil :wait nil :input :stream))
    (setf (tts-input handle) (sb-ext:process-input (tts-process handle)))
    (write-line (format nil "tts_set_punctuations all") (tts-input handle))
    (force-output (tts-input handle))))

(defun icon-file (icon)
  "Convert auditory icon name to a sound-file name."
  (declare (special *emacspeak*))
  (format nil "~a/sounds/pan-chimes/~a.wav"  *emacspeak* icon))
  

;;}}}
;;{{{Exported Functions

(defun tts-shutdown ()
  "Shutdown a TTS session."
  (let ((handle (tts)))
    (when (tts-input handle)
      (close (tts-input handle)))
    (setf (tts-input handle) nil)))

(defun tts-code (cmd)
  "Queue TTS code  to engine."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "c {~a}~%" cmd) 
    (finish-output i)))

(defun tts-icon (icon)
  "Queue auditory icon  to play."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "a {~a}~%" (icon-file icon))
    (finish-output i)))

(defun tts-queue (text)
  "Queue text to speak."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "q {~a}~%" text) 
    (finish-output i)))

(defun tts-pause (ms)
  "Send silence"
(let ((i (tts-input (tts))))
    (format i "sh {~a}~%" ms) 
    (finish-output i))  )

(defun tts-force ()
  "Speak all queued text."
  (let ((i (tts-input (tts))))
    (format i "d~%" )
    (finish-output i)))

(defun tts-stop ()
  "Stop speech."
  (let ((i (tts-input (tts))))
    (format i "s~%")
    (finish-output i)))

(defun tts-speak (text)
  "Speak text."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "q {~a}~%" text)
    (format i "d~%")
    (finish-output i)))

(defun tts-say (text)
  "Say text."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "tts_say {~a}~%" text)
    (format i "d~%")
    (finish-output i)))

(defun tts-speak-list (lines)
  "Speak an arbitrary number of lines."
  (tts)
  (mapc 'tts-queue lines)
  (force))

(defun tts-letter (text)
  "Speak letter."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "l ~a~%" text)
    (finish-output i)))

;;}}}
;;{{{Various: Intonation Functions:

(defun high-intonation ()
  "Generate H*"
  (tts-queue "[/]" ))

(defun low-intonation ()
  "Generate L*"
  (tts-queue "[\]" ))

(defun high-low-intonation ()
  "Generate Hl*"
  (tts-queue "[/\]"))

(defun comma-intonation ()
  "Generate a comma intonation"
  (tts-queue "[_,] "))

(defun period-intonation ()
  "Generate a period intonation"
  (tts-queue " "))

(defun exclamation  ()
  "Send an exclamation. "
  (tts-queue " "))

(defun interrogative  ()
  "Send an interrogative. "
  (tts-queue " "))

(defun primary-stress  ()
  "Send a  primary-stress. "
  (tts-queue " "))

(defun secondary-stress  ()
  "Send a  secondary-stress. "
  (tts-queue " "))

(defun exclamatory-stress  ()
  "Send an   exclamatory-stress. "
  (tts-queue " "))

(defun set-period-pause  (ms)
  "Set Period Pause."
  (tts-queue "[:pp ~a]" ms))


(defun set-comma-pause  (ms)
  "Set comma Pause."
  (tts-queue "[:pc ~a]" ms))

(defun subclause-boundary  ()
  "Send subclause boundary."
  (tts-queue "[)]" ms))
;;}}}
(provide 'tts)

;;{{{ end of file

;;; local variables:
;;; folded-file: t
 
;;; end:

;;}}}
