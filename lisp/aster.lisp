;;;   -*- Mode: LISP -*- ;;;

;; Place etc/10-aster.conf in  ~/.config/common-lisp/source-registry.conf.d/
;; for registering and finding packages.

(require "asdf")
(asdf:clear-source-registry)


(defvar *lisp-dir*
  (concatenate
   'string  "lisp/"
   (namestring (uiop:pathname-directory-pathname   #.   *load-truename*)))
  "directory under which lisp code is organized")

(defun aster ()
  "Load AsTeR modules and initialize system."
  (mapc #'asdf:load-system
        '(:parser :afl :pronounce :read-aloud :browse))
  (mapc #'asdf:load-system
        '("vavasis-book" "vanloan-book" "tcs-chicago"
          "rz-book" "norvig-book" "gries-book"
          "dennis-math-books" "cs611-notes"))
  ;; configure rules and  styles:
  (activate-rule 'stackrel 'default)
  (activate-rule 'overbrace 'default)
  (activate-rule 'underbrace 'default)
  (activate-style 'simple)
  (activate-style 'descriptive)
  (activate-rule 'log 'read-base-first)
  (activate-rule 'induction 'default)
  (activate-rule 'footnote 'float)
  (activate-style  'use-special-pattern)
  (setf *follow-cross-ref-wait* 0
        *get-label-wait* 0))

(aster)
