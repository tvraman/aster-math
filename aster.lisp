;;;   -*- Mode: LISP -*- ;;;

;; Place etc/10-aster.conf in  ~/.config/common-lisp/source-registry.conf.d/
;; for registering and finding packages.

(require "asdf")
(asdf:clear-source-registry)
(defvar *aster-root* nil
  "Project root.")

(defvar *lisp-dir*
  (let ((where
          (namestring
           (uiop:pathname-directory-pathname   #.   *load-truename*))))
    (setq *aster-root* where)
    (concatenate 'string where "lisp/"))
  "directory under which lisp code is organized")

(defun aster ()
  "Load AsTeR modules and initialize system."
  (asdf:load-system :parser)
  (asdf:load-system :afl)
  (asdf:load-system :pronounce)
  (asdf:load-system :read-aloud)
  (asdf:load-system :browse)
  (mapc #'asdf:load-system
        '(
          "vavasis-book"
          "vanloan-book"
          "tcs-chicago"
          "rz-book"
          "norvig-book"
          "gries-book"
          "dennis-math-books"
          "cs611-notes")))

(aster)

(defun aster-setup ()
  "Setup default reading rules and styles."
  (activate-rule 'stackrel 'default)
  (activate-rule 'overbrace 'default)
  (activate-rule 'underbrace 'default)
  (activate-style 'simple)
  (activate-style 'descriptive)
  (activate-rule 'log 'read-base-first)
  (activate-rule 'aster 'dont-bark)
  (setf *follow-cross-ref-wait* 0)
  (activate-rule 'induction 'default)
  (activate-rule 'footnote 'float)
  (activate-style  'use-special-pattern)
  (setf *follow-cross-ref-wait* 0
        *get-label-wait* 0))

(aster-setup)

(defun aster-file (filename)
  "Parse and read-aloud TeX file."
  (read-aloud (parse-article filename)))
