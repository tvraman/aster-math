(require "asdf")
;; Place 10-aster.conf in  ~/.config/common-lisp/source-registry.conf.d/aster.conf
;; for registering and finding packages

(asdf:clear-source-registry)

(defvar *lisp-code-directory*
  (let* 
          ((where
             (namestring  #. (or *compile-file-truename* *load-truename*)))
           (index (search "/"where :from-end t )))
        (concatenate
         'string 
         (subseq where 0 index)
         "/lisp-code/"))
  "directory under which lisp code is organized")

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
        *get-label-wait* 0))

(defun aster ()
  "Load AsTeR modules and initialize system."
  (pushnew :express   *features*)
  (asdf:load-system :parser)
  (asdf:load-system :clos-helper)
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
          "cs611-notes"))
  (aster-setup))

(defun read-aloud-file (filename)
  "Read aloud this file by first parsing it. "
  (declare (special *document*))
  (setf *document* (parse-article filename))
  (read-aloud *document* ))


(aster)
(aster-setup)
(setq s (parse-article "/home/raman/emacs/lisp/aster-math/short.tex"))
(setq d (parse-article "/home/raman/emacs/lisp/aster-math/test.tex"))


