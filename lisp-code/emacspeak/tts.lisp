;;; tts.lisp -- Common Lisp interface  to Emacspeak speech servers
;;; $Author: tv.raman.tv $
;;; Description:   Interface Common Lisp   to Emacspeak TTS servers
;;; Keywords: AsTeR, Emacspeak, Audio Desktop
;;; {  Copyright:

;;; Copyright (C)  2011, T. V. Raman<raman@cs.cornell.edu>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; }
;;; { Introduction:

;;; Commentary:
;;; Interface Common  Lisp  to Emacspeak TTS servers

;;; }
(in-package :user)
;;; { Settings

(defvar *emacspeak* "/usr/share/emacs/site-lisp/emacspeak/"
  "Root of Emacspeak installation.")

(defvar *tts-process* nil
  "Handle to tts server connection.")

(defvar *tts-engine* (tts-32-outloud)
  "Default TTS engine to use.")
(proclaim '(inline tts-process))
(defun tts-process ()
  "Return handle to TTS process."
  (declare (special *tts-process*))
  *tts-engine)
(defun tts-engine ()
  "Return default tts-engine."
  (declare (special *tts-engine*))
  *tts-engine)



(proclaim '(inline tts-process))
(defun tts-process ()
  "Return handle to TTS process."
  (declare (special *tts-process*))
  *tts-process)

(defun dtk-exp ()
  "Return name of dtk-exp server."
  (declare (special *emacspeak*))
  (concatenate 'string   *emacspeak* "servers/dtk-exp"))

(defun tts-outloud ()
  "Outloud tcl server"
  (declare (special *emacspeak*))
  (concatenate 'string   *emacspeak* "servers/outloud"))

(defun tts-32-outloud ()
  "Return location of 32-outloud server."
  (declare (special *emacspeak*))
  (concatenate 'string   *emacspeak* "servers/32-outloud"))

;;; }
;;; {Internal  Functions

(defun tts-open ()
  "Open a TTS session."
  (setf(tts-process)
        (ext:run-program
         (tts-engine) nil :wait nil  :input :stream)))

(defun tts-close ()
  "Close a TTS session."
  (let ((p (tts-process)))
  (when(and  (ext:process-p p)
             (ext:process-alive-p p))
    (ext:process-close p))
  (setf (tts-process) nil))

(defun tts-running-p ()
  "Is there a tts process up and running?"
  (let ((p (tts-process)))
  (and  p
       (ext:process-p p)
       (ext:process-alive-p p)))


(defun tts-queue (text)
  "Queue text to speak."
  (let ((p (tts-process)))
    (unless (and  p (ext:process-alive-p p))
      (setq p (tts-open)))
    (let ((i (ext:process-input p)))
      (write-line (format nil "q {~s}" text) i)
      (force-output i))))

(defun tts-force ()
  "Speak all queued text."
  (let ((i (ext:process-input (tts-process))))
    (write-line "d" i)
    (force-output i)))

;;; }
;;; {Exported Functions

(defun tts-stop ()
  "Stop speech."
  (let ((i (ext:process-input (tts-process))))
      (write-line "s"  i)
      (force-output i)))

(defun tts-speak (text)
  "Speak text."
  (unless (and  (tts-process)
                (ext:process-alive-p (tts-process)))
    (tts-open))
  (let ((i (ext:process-input (tts-process))))
      (write-line "s"  i)
      (force-output i)
    (write-line (format nil "q ~s\nd" text) i)
    (force-output i)))



(defun tts-speak-list (lines)
  "Speak an arbitrary number of lines."
  (tts-stop)
  (mapc 'tts-queue lines)
  (tts-force))

(defun tts-letter (text)
  "Speak letter."
  (unless (and  (tts-process)
                (ext:process-alive-p (tts-process)))
    (tts-open))
  (let ((i (ext:process-input (tts-process))))
    (write-line (format nil "l ~s" text) i)
    (force-output i)))

;;; }
(provide 'tts)

;;; { end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;; }
