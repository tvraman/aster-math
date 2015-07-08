(in-package :asdf)

;;; Contains objects from Vavasis's book.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "vavasis-book"
  :components
  ( 
   (:file "sv-object-definitions")
   (:file "sv-reading-rules" :depends-on ( "sv-object-definitions"))))
