
(in-package :asdf)


;;; Tue Feb 23 09:29:37 EST 1993

;;; Contains objects from  Norvig's book.
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "norvig-book"
  :components
  ( 
   (:file "norvig-object-definitions")
   (:file "norvig-reading-rules"
          :depends-on (  "norvig-object-definitions"))
   )
  )
