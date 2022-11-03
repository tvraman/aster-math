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
  (expand-file-name ".." (file-name-directory load-file-name))
  "Aster Project Root")

(defvar aster-lisp-dir
  (expand-file-name "lisp/" aster-root)
  "Directory where common-lisp files are stored.")

(defvar aster-setup
  (expand-file-name "setup.lisp" aster-root)
  "Common Lisp file loaded to configure Aster.")

;;}}}
;;{{{Helpers:

(defsubst a--code (code)
  "Return string representation of code; prin1-to-string."
  (prin1-to-string code))

(defsubst aster-check ()
  "Assert that Aster is ready"
  (cl-declare (special aster-ready))
  (cl-assert aster-ready t "First setup and start Aster"))

;;}}}
;;{{{Interactive Commands:

(defvar aster-ready nil
  "Flag to record if Aster is ready.")

(defun aster-post-startup ()
  "Prepare Aster once slime is ready."
  (cl-declare (special aster-setup aster-ready))
  (let ((welcome "(afl:tts-speak \"Welcome to Aster\") ")
        (afl-init "(afl:tts-init)"))
    (slime-load-file aster-setup)
    (accept-process-output)
    (sit-for 1)
    (slime-eval-save afl-init)
    (slime-eval-save welcome)
    (setq aster-ready t)))

(add-hook
 'slime-connected-hook
 'aster-post-startup
 'at-end)

(defun aster ()
  "Start Slime For AsTeR"
  (interactive)
  (while (not (slime-connected-p))
    (slime)
    (sit-for 1))
  (accept-process-output))

(defun aster-cmd (sexp)
  "Run Aster command,  a sexp."
  (interactive (list (read-minibuffer "Aster:")))
  (slime-eval-save (a--code sexp)))

(defun aster-file (file)
  "Run Aster on specified file."
  (interactive "fFile: ")
  (aster-check)
   (aster-cmd `(aster-file ,file)))


(defun aster-region (start end)
  "Send region to aster to be read out."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (file
         (or (get-text-property start 'aster-file)
             (make-temp-file "aster" nil ".tex"))))
    (unless (get-text-property (point) 'aster-file)
      (put-text-property start end 'aster-file file)
      (set-buffer-modified-p nil))
    (with-temp-file file
      (insert "\\begin{document}\n")
      (insert text)
      (insert "\\end{document}\n"))
    (aster-file file)))

;;}}}
(provide 'aster)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}