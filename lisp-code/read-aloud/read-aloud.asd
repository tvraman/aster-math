(in-package :asdf)
(unless (find-package :read-aloud) (make-package :read-aloud))

(defsystem "read-aloud"
  :description "AsTeR's Audio Formatting Rules"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   
   (:file "cardinal-numbers" )
   (:file "get-user-feedback" )
   (:file "rule-and-style-macros")
   (:file "read-aloud"
    :depends-on ("rule-and-style-macros"
                 "reading-state-definitions"))
   (:file "time-readings" :depends-on  ("read-aloud"))
   (:file "handling-reading-states")
   (:file  "reading-state-definitions"
    :depends-on ("handling-reading-states" ))
   ))
