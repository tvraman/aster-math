(load "/usr/share/common-lisp/source/cl-asdf/asdf.lisp")
(defvar *lisp-code-directory*
  (merge-pathnames "emacs/lisp/aster-math/lisp-code/" (user-homedir-pathname))
  "directory under which lisp code is organized")
(defvar *books*
  '(
    "vavasis-book"
    "vanloan-book"
    "tcs-chicago"
    "rz-book"
    "norvig-book"
    "gries-book"
    "dennis-math-books"
    "cs611-notes"
    )
  "List of books")

(let ((aster *lisp-code-directory*))
  ;(load (merge-pathnames "lisp-utilities/query"  aster))
  (loop
    for d in
       (append 
    '("latex-parser" "tts" "clos-helper"
      "acss" "afl" "afl/pronounce" "afl/total-space" "read-aloud" "browse")
    *books*)
    do
       (pushnew  (merge-pathnames d aster) asdf:*central-registry*)))

(defun aster-setup ()
  "Setup default reading rules and styles."
  (pushnew :express   *features*)
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
        *get-label-wait* 0)
  )

(defun aster ()
  "Load AsTeR modules and initialize system."
  (asdf:load-system :parser)
  (asdf:load-system :clos-helper)
  (asdf:load-system :afl)
  (asdf:load-system :pronounce)
  (asdf:load-system :total-space)
  (asdf:load-system :read-aloud)
  (asdf:load-system :browse)
  (mapc #'asdf:load-system *books*)
  (aster-setup))

(defun read-aloud-file (filename)
  "Read aloud this file by first parsing it. "
  (declare (special *document*)) 
  (setf *document* (parse-article filename))
  (read-aloud *document* ))


;;; local variables:
;;; mode: lisp
;;; end:
