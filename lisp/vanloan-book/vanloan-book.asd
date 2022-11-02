(in-package :asdf)

;;; Contains objects from Vanloan's book.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "vanloan-book"
  :components
  ((:file "cv-object-definitions")
   (:file "cv-reading-rules" :depends-on ( "cv-object-definitions"))))
