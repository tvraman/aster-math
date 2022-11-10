;;; aster.el - AsTeR UI  -*- lexical-binding: t; -*-
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction:

;;  Commentary:
;;  See design.org for the overall design.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'slime)
(require 'repeat)
(require 'calc)
(require 'texmathp)
;;}}}
;;{{{ Configure locations:

(defvar aster-root
  (expand-file-name ".." (file-name-directory load-file-name))
  "Aster Project Root")

(defvar aster-setup
  (expand-file-name "lisp/aster.lisp" aster-root)
  "Common Lisp file loaded to configure Aster.")

;;}}}
;;{{{Helpers:

(defsubst a--code (code)
  "Return string representation of code; prin1-to-string."
  (prin1-to-string code))

(defsubst a--pa-index (pattern)
  "Index of input sink from list-sink-inputs for app matching pattern."
  (string-trim
   (shell-command-to-string
    (format
     (concat
      "pacmd list-sink-inputs | grep -i -B 15 %s |"
      "grep index | cut -d ':' -f 2")
     pattern))))

(defvar aster-ready nil
  "Flag to record if Aster is ready.")

(defsubst aster-check ()
  "Check that Aster is ready"
  (cl-declare (special aster-ready))
  (unless
      (and  aster-ready
            (condition-case nil
                (slime-process)
              (error nil)))
    (aster)))

(defsubst aster-eval (string)
  "Like slime-eval-save."
  (let ((buffer (get-buffer-create "aster-temp")))
    (with-current-buffer buffer
      (slime-eval-async `(swank:eval-and-grab-output ,string)
        (lambda (_result) t)))))

;;}}}
;;{{{Guess Math Input:

(declare-function calc-kill "calc-yank" (flag no-delete))

(defsubst aster-guess-calc ()
  "Guess expression to speak in calc buffers. Set calc-language to latex "
  (cl-declare (special calc-last-kill ))
  (cl-assert (eq major-mode 'calc-mode) nil "Not in a calc buffer.")
  (calc-kill 1 'no-delete)
  (substring (car calc-last-kill) 2))

(declare-function emacspeak-sage-get-output-as-latex "emacspeak-sage" nil)

(defun aster-guess-sage ()
  "Guess expression to speak in sage-mode buffers."
  (cl-assert
   (eq major-mode 'sage-shell:sage-mode) nil "This is not a Sage buffer.")
  (sit-for 0.1)
  (emacspeak-sage-get-output-as-latex))

(defun aster-guess-tex ()
  "Extract math content around point in TeX/LaTeX."
  (cl-declare (special texmathp-why))
  (cl-assert (require 'texmathp) nil "Install package auctex")
  (when (texmathp)
    (let ((delimiter (car texmathp-why))
          (start (cdr texmathp-why))
          (begin nil)
          (end nil))
      (cond
       ;; $ and $$
       ((or (string= "$" delimiter)
            (string= "$$" delimiter))
        (save-excursion
          (goto-char start)
          (forward-char (length delimiter))
          (setq begin (point))
          (skip-syntax-forward "^$")
          (setq end (point))
          (buffer-substring-no-properties begin end)))
       ;; \( and \[
       ((string= "\\(" delimiter)
        (goto-char start)
        (setq begin (+ start  2))
        (search-forward "\\)")
        (setq end (- (point) 2))
        (buffer-substring-no-properties begin end))
       ((string= "\\[" delimiter)
        (goto-char start)
        (setq begin (+ start  2))
        (search-forward "\\]")
        (setq end (- (point) 2))
        (buffer-substring-no-properties begin end))
       ;; begin equation
       ((string= "equation" delimiter)
        (goto-char start)
        (forward-char (length "\\begin{equation}"))
        (setq begin (point))
        (search-forward "\\end{equation}")
        (backward-char (length "\\begin{equation}"))
        (setq end (point))
        (buffer-substring-no-properties begin end))
       (t nil)))))

(defun aster-guess ()
  "Examine current mode and context  etc. to guess Math content to read."
  (aster-check)
  (cond
   ((eq major-mode 'calc-mode)
    (aster-guess-calc))
   ((eq major-mode 'sage-shell:sage-mode)
    (aster-guess-sage))
   ((and (memq major-mode '(tex-mode plain-tex-mode latex-mode ams-tex-mode))
         (featurep 'texmathp))
    (aster-guess-tex))
   ((and
     (eq major-mode 'eww-mode)
     (not
      (string-equal
       (get-text-property (point) 'shr-alt)
       "No image under point")))
    (get-text-property (point) 'shr-alt))
   (t
    (read-from-minibuffer
     "LaTeX: " nil nil nil nil
     (when mark-active (buffer-substring (region-beginning)(region-end)))))))

;; ###autoload

;;}}}
;;{{{Running Aster:
(defun aster ()
  "Load and start Aster"
  (interactive)
  (while (not (slime-connected-p))
    (slime)
    (sit-for 1)))

(defun aster-post-startup ()
  "Prepare Aster once slime is ready."
  (cl-declare (special aster-setup aster-ready))
  (let ((welcome "(afl:tts-speak \"Welcome to Aster\") ")
        (afl-init "(afl:tts-init)"))
    (slime-load-file aster-setup)
    (accept-process-output)
    (sit-for 1)
    (aster-eval afl-init)
    (aster-eval welcome)
    (setq aster-ready t)))

(add-hook
 'slime-connected-hook
 'aster-post-startup
 'at-end)

(defun aster-cmd (sexp)
  "Run Aster command,  a sexp, after first stopping speech."
  (aster-eval
   (a--code
    `(progn (afl:tts-stop) ,sexp))))

;;}}}
;;{{{Reading Commands:

(defun aster-math (latex)
  "Send a LaTeX expression to Aster,
 guess  based on context. "
  (interactive (list (aster-guess)))
  (aster-check)
  (when (or (null latex) (string= "" latex))
    (setq latex (read-from-minibuffer "Enter expression:")))
  (aster-text latex))

(defun aster-file (file)
  "Run Aster on specified file."
  (interactive "fFile: ")
  (aster-check)
  (aster-cmd `(read-aloud (parse-latex-file ,file))))

(defun aster-record ()
  "Record Aster's reading of current node.
Output is found in aster-rootp/tests/aster.ogg which will be overwritten"
  (interactive )
  (let ((index "")
        (output (expand-file-name "tests/aster.ogg" aster-root))
        (move "pacmd move-sink-input %s snoop ")
        (record "parec -d snoop.monitor | oggenc -o %s -r - &"))
    (aster-check)
    (aster-current)
    (sit-for 0.4)
    (setq index (a--pa-index "DEC"))
    (unless (zerop (length index))
      (shell-command (format move index ))
      (shell-command (format record output))
      (message "Recording. Remember to kill parec"))))

(defun aster-region (start end)
  "Read region using Aster."
  (interactive "r")
  (aster-eval
   (a--code
    `(read-aloud
      (parse-latex-string
       ,(concat
         "\\begin{document}"
         (buffer-substring-no-properties start end)
         "\\end{document}"))))))

(defun aster-text (text)
  "Send text as LaTeX  Math to aster to be read out."
  (with-temp-buffer
    (insert "\\begin{document}$")
    (insert text)
    (insert "$\\end{document}")
    (aster-eval
     (a--code
      `(read-aloud (parse-latex-string ,(buffer-string)))))))

;;}}}
;;{{{Navigators:

(defun aster-current ()
  "Aster current node."
  (interactive)
  (aster-cmd '(read-current)))

(defun aster-to-attributes ()
  "Move to attributes"
  (interactive)
  (aster-cmd '(move-to-attributes )))

(defun aster-to-children ()
  "Move to children"
  (interactive)
  (aster-cmd '(move-to-children )))

(defun aster-to-abstract ()
  "Move to abstract"
  (interactive)
  (aster-cmd '(move-to-abstract )))

(defun aster-parent ()
  "Move to and read parent."
  (interactive)
  (aster-cmd '(read-parent )))

(defun aster-rest ()
  "Read rest of current node."
  (interactive)
  (aster-cmd '(read-rest  *read-pointer* )))

(defun aster-to-subscript ()
  "Move to subscript"
  (interactive)
  (aster-cmd '(move-to-subscript )))

(defun aster-to-superscript ()
  "Move to superscript"
  (interactive)
  (aster-cmd '(move-to-superscript )))

(defun aster-above ()
  "If in a matrix or other tabular structure,
 move the current selection to the element above it, and read it."
  (interactive)
  (aster-cmd '(move-to-above )))

(defun aster-below ()
  "If in a matrix or other tabular structure,
 move the current selection to the element below it, and read it."
  (interactive)
  (aster-cmd '(move-to-below )))

(defun aster-to-bookmark ()
  "Move to bookmark"
  (interactive)
  (aster-cmd '(goto-bookmark )))

(defun aster-to-cross-ref ()
  "Move to cross-ref"
  (interactive)
  (aster-cmd '(read-follow-cross-ref )))

(defun aster-to-left ()
  "Move left"
  (interactive)
  (aster-cmd '(move-back )))

(defun aster-to-right ()
  "Move right"
  (interactive)
  (aster-cmd '(move-forward )))

(defun aster-to-up ()
  "Move up"
  (interactive)
  (aster-cmd '(move-up )))

(defun aster-to-contents ()
  "Move to contents"
  (interactive)
  (aster-cmd '(move-to-contents )))

(defun aster-previous ()
  "Move to previous and read it."
  (interactive)
  (aster-cmd '(read-previous )))

(defun aster-next ()
  "Move to next and read it."
  (interactive)
  (aster-cmd '(read-next )))

(defun aster-stop ()
  "Stop speech"
  (interactive)
  (aster-cmd '(afl:tts-stop )))

(defun aster-to-top (prefix)
  "Move to root of math expression, or to document root."
  (interactive "P")
  (aster-stop)
  (if prefix
      (aster-eval
       (a--code
        '(progn
           (setf *read-pointer* *document*)
           (summarize *document*))))
    (aster-eval (a--code '(move-to-top-of-math)))))

;;}}}
;;{{{Key Bindings:

(defvar  aster-keymap nil
  " aster Keymap")

(define-prefix-command 'aster-keymap   'aster-keymap)
(global-set-key  (kbd "C-; SPC") 'aster-keymap)

(defsubst aster-keymap-bindings-update (keymap bindings)
  "Update keymap with  list of bindings."
  (cl-loop
   for binding in bindings
   do
   (define-key keymap (kbd (cl-first binding)) (cl-second binding))))

(defcustom aster-keys
  '(
    ("." aster-current)
    ("A" aster-to-attributes)
    ("C" aster-children)
    ("C-a" aster-to-abstract)
    ("C-r" aster-record)
    ("r" aster-region)
    ("P" aster-parent )
    ("SPC" aster-rest)
    ("^" aster-to-superscript )
    ("_" aster-to-subscript)
    ("d" aster-below)
    ("f"aster-file)
    ("g" aster-to-cross-ref)
    ("h" aster-to-left)
    ("i" aster-to-contents)
    ("j" aster-to-children )
    ("k" aster-to-up)
    ("l" aster-to-right)
    ("m" aster-math)
    ("n" aster-next)
    ("p" aster-previous)
    ("s" aster-stop)
    ("t" aster-to-top)
    ("u" aster-above)
    ("<down>" aster-to-children )
    ("<left>" aster-to-left)
    ("<right>" aster-to-right)
    ("<up>" aster-to-up))

  "Aster key bindings. "
  :group 'aster
  :type '(repeat
          :tag "Emacspeak Super Keymap"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (aster-keymap-bindings-update aster-keymap  val)
      (set-default sym
                   (sort
                    val
                    #'(lambda (a b) (string-lessp (car a) (car b)))))))

;;}}}
;;{{{Setup Repeat Mode

(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'aster-keymap)))
 aster-keymap)

;;}}}
(provide 'aster)
;;{{{ end of file

;;  local variables:
;;  folded-file: t
;;  end:

;;}}}
