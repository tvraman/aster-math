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
(require  'slime-repl)
(require 'repeat)
(require 'calc)
(require 'texmathp)
;;}}}
;;{{{Customizations:

(defgroup aster nil
  "Audio System For Technical Readings"
  :link
  '(url-link :tag "Demo"
    "https://emacspeak.sourceforge.net/raman/aster/2022-aster.ogg"
    :help-echo "AsTeR Demo")
  :link
  '(url-link :tag "Source"
    "https://github.com/tvraman/aster-math"
    :help-echo "AsTeR Source Code")
  :group 'applications)

(defcustom aster-bind-arrows nil
  "Turn this on to have AsTeR use arrows for navigation."
  :type 'boolean
  :group 'aster)

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

(defalias 'a--code 'prin1-to-string)

(defsubst a--pa-index (pattern)
  "Index of input-sink used by app that matches  pattern.
Value is derived from `pacmd list-sink-inputs'."
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
  "Check that Aster is ready. Start slime if needed."
  (cl-declare (special aster-ready))
  (unless
      (and  aster-ready
            (condition-case nil (slime-process) (error nil))
            (aster))
    t))

(defsubst aster-eval (string)
  "Like slime-eval-save but ignores result."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (lambda (_result) t)))

(defsubst a--wrap-doc (latex)
  "Wrap text in document start/end markup."
  (concat
   "\\begin{document}"
   latex
   "\\end{document}"))

(defsubst a--wrap-math (latex)
  "Wrap text in math document start/end markup."
  (concat
   "\\begin{document}$"
   latex
   "$\\end{document}"))

;;}}}
;;{{{Guess Math Input:

(declare-function calc-kill "calc-yank" (flag no-delete))

(defsubst aster-guess-calc ()
  "Guess expression to speak in calc buffers. Set calc-language to latex "
  (cl-declare (special calc-last-kill ))
  (cl-assert (eq major-mode 'calc-mode) nil "Not in a calc buffer.")
  (calc-kill 1 'no-delete)
  (string-trim (substring (car calc-last-kill) 2)))

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
  "Examine current mode and context to guess Math content to read."
  (aster-check)
  (cond
    ((eq major-mode 'calc-mode) (aster-guess-calc))
    ((eq major-mode 'sage-shell:sage-mode) (aster-guess-sage))
    ((and (memq major-mode '(tex-mode plain-tex-mode latex-mode))
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
  "Load and start Aster.
Aster commands are invoked by first pressing the prefix-key  \\[aster-keymap].
If you activate \\[repeat-mode] in newer versions of Emacs, you need
  press the prefix key only the first time when invoking a sequence of
  Aster commands.

The following commands are available on this prefix once aster is running:

\\{aster-keymap}"
  (interactive)
  (while (not (slime-connected-p))
         (slime)
         (sit-for 1)))


(defun aster-restart  ()
  "Restart lisp and load Aster. "
  (interactive)
  (slime-quit-lisp)
  (sit-for 1)
  (aster))

(defun aster-post-startup ()
  "Prepare Aster once slime is ready."
  (cl-declare (special aster-setup aster-ready))
  (let ((welcome "(afl:tts-speak \"Welcome to Aster\") ")
        (afl-init "(afl:tts-init)"))
    (slime-load-file aster-setup)
    (sit-for 1)
    (aster-eval afl-init)
    (aster-eval welcome)
    (slime-repl-eval-string (a--code '(in-package :aster)))
    (setq aster-ready t)))

(add-hook
 'slime-connected-hook
 'aster-post-startup
 'at-end)

(defun aster-cmd (sexp)
  "Run Aster command,  a sexp, after first stopping speech."
  (aster-eval
   (a--code `(progn (afl:tts-stop) ,sexp))))

;;}}}
;;{{{Reading Commands:

(defun aster-math (latex)
  "Send  LaTeX  text (no math delimiters) to Aster guessed from     context. "
  (interactive (list (aster-guess)))
  (aster-check)
  (when (or (null latex) (string= "" latex))
    (setq latex (read-from-minibuffer "Enter expression:")))
  (aster-cmd
   `(aster:text
     ,(a--wrap-math latex)
     :id ',(gensym "aster-")
     :title ,(buffer-name))))

(defun aster-file (file)
  "Run Aster on specified file."
  (interactive "fFile: ")
  (aster-check)
  (aster-cmd
   `(aster:file ,file)))

;; Make sure to first setup device <snoop> via default.pa for
;; Pulseaudio

(defsubst a--snoop-p ()
  "Check if snoop device is present."
  (zerop (shell-command "pacmd list-sinks | grep snoop ")))

(defvar-local aster-recording-p nil
  "Flag that says if we're recording.")

(defun aster-record ()
  "Record Aster's reading of current node.
  Output is found in aster-rootp/tests/aster.ogg which will be
  overwritten.  Calling aster-top bound to \\[aster-stop]stops
  the recording. See
  https://github.com/tvraman/emacspeak/blob/master/etc/pulse/default.pa#L19
  for setting up device <snoop>."
  (interactive )
  (cl-declare (special aster-recording-p))
  (cl-assert (a--snoop-p) t "First set up snoop device." )
  (let ((index "")
        (output (expand-file-name "tests/aster.ogg" aster-root))
        (move "pacmd move-sink-input %s snoop; ")
        (record "parec -d snoop.monitor | oggenc -o %s -r - &"))
    (aster-check)
    (aster-current)
    (sit-for 0.05)
    (setq index (a--pa-index "DEC"))
    (unless (zerop (length index))
      (shell-command
       (concat
        (format move index )
        (format record output)))
      (setq aster-recording-p t))))

(defun aster-region (start end)
  "Read region using Aster."
  (interactive "r")
  (aster-cmd
   `(aster:text
     ,(a--wrap-doc (buffer-substring-no-properties start end))
     :title ,(buffer-name))))

;;}}}
;;{{{Navigation Commands:

(defun aster-current ()
  "Aster current node."
  (interactive)
  (aster-cmd '(aster:read-current)))

(defun aster-to-attributes ()
  "Move to attributes"
  (interactive)
  (aster-cmd '(aster:move-to-attributes )))

(defun aster-to-children ()
  "Move to children"
  (interactive)
  (aster-cmd '(aster:move-to-children )))

(defun aster-to-abstract ()
  "Move to abstract"
  (interactive)
  (aster-cmd '(aster:move-to-abstract )))

(defun aster-parent ()
  "Move to and read parent."
  (interactive)
  (aster-cmd '(aster:read-parent )))

(defun aster-to-subscript ()
  "Move to subscript"
  (interactive)
  (aster-cmd '(aster:move-to-subscript )))

(defun aster-to-superscript ()
  "Move to superscript"
  (interactive)
  (aster-cmd '(aster:move-to-superscript )))

(defun aster-to-left ()
  "Move left"
  (interactive)
  (aster-cmd '(aster:move-back )))

(defun aster-to-right ()
  "Move right"
  (interactive)
  (aster-cmd '(aster:move-forward )))

(defun aster-to-up ()
  "Move up"
  (interactive)
  (aster-cmd '(aster:move-up )))

(defun aster-to-contents ()
  "Move to contents"
  (interactive)
  (aster-cmd '(aster:move-to-contents )))

(defun aster-previous ()
  "Move to previous and read it."
  (interactive)
  (aster-cmd '(aster:read-previous )))

(defun aster-next ()
  "Move to next and read it."
  (interactive)
  (aster-cmd '(aster:read-next )))

(defun aster-stop ()
  "Stop speech. Stops recording if we were recording."
  (interactive)
  (cl-declare (special aster-recording-p))
  (aster-cmd '(afl:tts-stop ))
  (when aster-recording-p
    (shell-command "pidof parec && pkill parec"))
  (setq aster-recording-p nil))

(defun aster-to-document-root ()
  "Move to   document root."
  (interactive )
  (aster-stop)
  (aster-cmd '(aster:move-to-doc-root)))

(defun aster-to-math-root ()
  "Move to   math  root."
  (interactive )
  (aster-stop)
  (aster-cmd '(aster:move-to-math-root)))

;;}}}
;;{{{Key Bindings:

(defvar  aster-keymap nil
  " aster Keymap")

(define-prefix-command 'aster-keymap   'aster-keymap)
(global-set-key  (kbd "C-; SPC") 'aster-keymap)
(global-set-key  (kbd "C-' a") 'aster-keymap)

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
    ("C-a" aster-to-abstract)
    ("C-r" aster-record)
    ("^" aster-to-superscript )
    ("_" aster-to-subscript)
    ("f"aster-file)
    ("h" aster-to-left)
    ("i" aster-to-contents)
    ("j" aster-to-children )
    ("k" aster-to-up)
    ("l" aster-to-right)
    ("m" aster-math)
    ("n" aster-next)
    ("p" aster-previous)
    ("r" aster-region)
    ("s" aster-stop)
    ("T" aster-to-document-root)
    ("t" aster-to-math-root)
    ("u" aster-parent ))

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

(when aster-bind-arrows
  (cl-loop
   for b in
   '(("<down>" aster-to-children )
     ("<left>" aster-to-left)
     ("<right>" aster-to-right)
     ("<up>" aster-to-up))
   do
   (define-key aster-keymap  (cl-first b) (cl-second b))))

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
