;;;   -*- Mode: LISP -*- ;;;
(require "asdf")
(asdf:initialize-source-registry
  `(:source-registry
     (:tree "/home/raman/emacs/lisp/aster-math/lisp")
     :inherit-configuration))

(unless (find-package :aster)
  (make-package :aster :use '(:common-lisp :sb-ext )))

(in-package :aster)

(defun lexer ()
  "Return pathname of lexer."
  (let ((base (uiop:pathname-directory-pathname   #. *load-truename*)))
    (merge-pathnames "lexer/lispify" base)))

(defun aster ()
  "Load AsTeR modules and initialize system."
  (when (uiop:getenv "ASTER_TTS")
    (setf (uiop:getenv "PULSE_SINK") (uiop:getenv "ASTER_TTS")))
  (let ((*print-case* :downcase))
    (mapc #'asdf:load-system '(:parser :afl  :read-aloud :browse))
    (format nil "Ready to talk")))

(defun aster-books ()
  "Modules for reading books I used at Cornell."
  (mapc
   #'asdf:load-system
   '( :vavasis-book :vanloan-book :tcs-chicago :rz-book
     :norvig-book :gries-book :dennis-math-books :cs611-notes)))

(aster)
;(aster-books)
