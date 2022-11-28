;;;   -*- Mode: LISP -*- ;;;
(require "asdf")

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(uiop:pathname-directory-pathname   #. *load-truename*))
   :inherit-configuration))

(unless (find-package :aster)
  (make-package :aster :use '(:common-lisp :sb-ext )))

(in-package :aster)

(defun lexer ()
  "Return pathname of lexer."
  (let ((base (uiop:pathname-directory-pathname   #. *load-truename*)))
    (merge-pathnames "lexer/lispify" base)))

(defun aster ()
  "Load AsTeR modules.
Use audio-device ASTER_TTS if set in the environment."
  (when (uiop:getenv "ASTER_TTS")
    (setf (uiop:getenv "PULSE_SINK") (uiop:getenv "ASTER_TTS")))
  (let ((*print-case* :downcase))
    (mapc #'asdf:load-system '(:parser :afl  :read-aloud :browse))))

(defun aster-books ()
  "Modules for reading books I used at Cornell."
  (mapc
   #'asdf:load-system
   '( :vavasis-book :vanloan-book :tcs-chicago :rz-book
     :norvig-book :gries-book :dennis-math-books :cs611-notes)))

(aster)
