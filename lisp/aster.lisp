;;;   -*- Mode: LISP -*- ;;;

;; Place etc/10-aster.conf in  ~/.config/common-lisp/source-registry.conf.d/
;; for registering and finding packages.

(require "asdf")
(asdf:clear-source-registry)
(unless (find-package :aster)
  (make-package :aster :use '(:cl :common-lisp :sb-ext )))

(in-package :aster)
(export '(aster))

(defvar *lisp-dir*
  (namestring (uiop:pathname-directory-pathname   #.   *load-truename*))
  "directory under which lisp code is organized")

(defun aster ()
  "Load AsTeR modules and initialize system."
  (when (uiop:getenv "ASTER_TTS")
    (setf (uiop:getenv "PULSE_SINK") (uiop:getenv "ASTER_TTS")))
  (let (                         ; (*muffled-warnings* 'style-warning)
        (*print-case* :downcase))
    (mapc #'asdf:load-system
          '(:parser :afl  :read-aloud :browse))
    (setf *follow-cross-ref-wait* 0
          *get-label-wait* 0)
    (format nil "Ready to talk")))

(defun aster-books ()
  "Modules for reading books I used at Cornell."
  (mapc #'asdf:load-system
          '( :vavasis-book :vanloan-book :tcs-chicago :rz-book
            :norvig-book :gries-book :dennis-math-books
            :cs611-notes)))

(aster)
