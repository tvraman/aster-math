;;;   -*- Syntax: Common-Lisp; Package: tts ; Mode: LISP -*-    ;;;
;;; tts.lisp -- Common Lisp interface to Emacspeak speech servers
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
;;{{{Package Exports:

(in-package :cl-user)

(defpackage :tts
  (:export
 #:code #:queue #:speak #:letter #:speak-list #:icon
 #:pause #:stop #:force
 #:init #:shutdown)
  )
(in-package :tts)

;;}}}
;;{{{ Setup:

;;; A TTS structure holds the engine name, process handle, and input/output streams.
(defstruct tts engine process input output )

(defvar *emacspeak* "/home/raman/emacs/lisp/emacspeak"
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

(defun init (&key (engine "dtk-soft"))
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
;;{{{Internal Functions

(defun tts-open ()
  "Open a TTS session."
  (let ((handle (tts)))
    (setf (tts-process handle)
          (sb-ext:run-program
           *tts-engine* nil :wait nil :input :stream)
          (setf (tts-input handle) (sb-ext:process-input (tts-process handle))))
    (write-line (format nil "tts_set_punctuations all") (tts-input handle))
    (force-output (tts-input handle))))

(defun icon-file (icon)
  "Convert auditory icon name to a sound-file name."
  (declare (special *emacspeak*))
  (format nil "~a/sounds/pan-chimes/~a.wav"  *emacspeak* icon))
  

;;}}}
;;{{{Exported Functions

(defun shutdown ()
  "Shutdown a TTS session."
  (let ((handle (tts)))
    (when (tts-input handle)
      (close (tts-input handle)))
    (setf (tts-input handle) nil)))

(defun code (cmd)
  "Queue TTS code  to engine."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "c {~a}~%" cmd) 
    (finish-output i)))

(defun icon (icon)
  "Queue auditory icon  to play."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "a {~a}~%" (icon-file icon))
    (finish-output i)))

(defun queue (text)
  "Queue text to speak."
  (let ((i (tts-input (tts))))
    (unless i (setq i (tts-open)))
    (format i "q {~a}~%" text) 
    (finish-output i)))

(defun pause (ms)
  "Send silence"
(let ((i (tts-input (tts))))
    (format i "sh {~a}~%" ms) 
    (finish-output i))  )

(defun force ()
  "Speak all queued text."
  (let ((i (tts-input (tts))))
    (format i "d~%" )
    (finish-output i)))

(defun stop ()
  "Stop speech."
  (let ((i (tts-input (tts))))
    (format i "s~%")
    (finish-output i)))

(defun speak (text)
  "Speak text."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "q {~a}~%" text)
    (format i "d~%")
    (finish-output i)))

(defun speak-list (lines)
  "Speak an arbitrary number of lines."
  (tts)
  (mapc 'tts-queue lines)
  (force))

(defun letter (text)
  "Speak letter."
  (unless (tts-input (tts)) (tts-open))
  (let ((i (tts-input (tts))))
    (format i "l ~a~%" text)
    (finish-output i)))

;;}}}
(provide 'tts)

;;{{{ end of file

;;; local variables:
;;; folded-file: t
 
;;; end:

;;}}}
