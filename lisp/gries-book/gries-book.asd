(in-package :asdf)

;;; Contains objects from Gries's book.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "gries-book"
  :components
  ((:file "dg-object-definitions")
              (:file "dg-reading-rules"
                     :depends-on (  "dg-object-definitions"))))
