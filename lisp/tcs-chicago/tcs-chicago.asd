(in-package :asdf)

;;; Contains objects from tcs chicago journal 
;;; One file will have object definitions, and the rest reading rules.

(defsystem "tcs-chicago"
  :components
  ((:file "tcs-object-definitions")
   (:file "tcs-reading-rules")))
