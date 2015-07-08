(in-package :asdf)

;;; Contains objects from Zippel's book.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "rz-book"
  :components
  ( 
   (:file "rz-object-definitions")
   (:file "rz-reading-rules"
          :depends-on (  "rz-object-definitions"))))
