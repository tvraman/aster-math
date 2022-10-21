;;;   -*- Syntax: Common-Lisp; Package: tts ; Mode: LISP -*-    ;;;
;;; dtk.lisp: Common Lisp Interface To Software Dectalk
;;; $Author: tv.raman.tv $
;;; Description: Interface Common Lisp to Software Dectalk
;;; Keywords: AsTeR, Emacspeak, Audio Desktop, Dectalk
;;{{{Copyright:

;;; Copyright (C) 1992 -- 2022, T. V. Raman<tv.raman.tv@gmail.com>
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
;;{{{Introduction:

;;; Commentary:
;;; Interface Common Lisp to Software Dectalk 

;;}}}
;;{{{Package Exports:
(in-package :cl-user)

(defpackage :dectalk
  (:use :common-lisp)
  (:export))

(in-package :dectalk)


;;}}}
;;{{{Setup:

;;; A tts structure holds the process handle, and input/output
;;; streams.

(defstruct tts  process input output )

(defvar *tts* nil
  "Handle to tts server connection.")

(defun init  ()
  "Initialize TTS  system."
  (declare (special *tts*))
  (setq *tts* (make-tts ))
  (tts-open))

(proclaim '(inline tts))
(defun tts ()
  "Return handle to TTS server."
  (declare (special *tts*))
  *tts*)

;;}}}
;;{{{Internal Functions

(defun tts-open ()
  "Open a TTS session."
  (let ((handle (tts)))
    (setf(tts-input handle)
         (ext:make-pipe-output-stream (tts-engine handle) :buffered nil))))

;;}}}
;;{{{Exported Functions

(defun shutdown ()
  "Shutdown a TTS session."
  (let ((handle (tts)))
    (when (tts-input handle)
      (close (tts-input handle)))
    (setf (tts-input handle) nil)))



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

;;}}}
(provide 'tts)

;;{{{end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

