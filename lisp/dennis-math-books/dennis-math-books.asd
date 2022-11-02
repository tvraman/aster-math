
(in-package :asdf)

;;; Contains objects from Dennis-Math-Books's
;;; One file will have object definitions, and the rest reading rules.

;;; First load complete read-aloud system then this system 

(defsystem "dennis-math-books"
  :components
  ((:file "kd-object-definitions")
   (:file "kd-reading-rules"
          :depends-on (  "kd-object-definitions"))))
