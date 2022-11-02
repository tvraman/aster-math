;;; aster.el - Load And Configure -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Load and Configure Aster
;;; Keywords: Audio System For Technical Readings (AsTeR
;;{{{  Copyright:

;;;Copyright (C) 2022, T. V. Raman
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
;;; MERCHANTABILITY or FITN<SKELETON> FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; See design.org for the overall design.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'slime)

;;}}}
;;{{{ Configure locations:

(defvar aster-root
  (expand-file-name "../" (file-name-directory load-file-name))
  "Directory where common-lisp files are stored.")

(defvar aster-lisp-dir
  (expand-file-name "lisp/" aster-root)
  "Directory where common-lisp files are stored.")

(defvar aster-setup
  (expand-file-name "aster-setup.lisp" aster-root)
  "Common Lisp file loaded to configure Aster.")

;;}}}
;;{{{Helpers:

(defsubst aster-code-to-string (code)
  "Return string representation of code."
  (prin1-to-string code))

;;}}}
;;{{{Interactive Functions:
(defun aster-post-startup ()
  "Announce AsTeR is ready by starting TTS."
  (let ((welcome "(afl:tts-speak \"Welcome to Aster\") ")
        (afl-init "(afl:tts-init)"))
    (slime-eval-save afl-init)
    (slime-eval-save welcome)))

(defun aster ()
  "Load and configure AsTeR."
  (interactive)
  (cl-declare (special aster-setup slime-default-connection))
  (while (not slime-default-connection)
    (slime)
    (sit-for 1))
  (slime-load-file aster-setup)
  (sit-for 1)
  (accept-process-output))

(defun aster-cmd (string)
  "Read Aster sexp from minibuffer and run it."
  (interactive (list (read-from-minibuffer "Aster Sexp:")))
  (slime-eval-save string))

;;}}}
(provide 'aster)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
