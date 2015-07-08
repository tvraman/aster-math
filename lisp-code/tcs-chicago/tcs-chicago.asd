(in-package :asdf)

;;; Contains objects from tcs chicago journal 
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "tcs-chicago"
  :components
  ( 
   (:file "tcs-object-definitions")))
