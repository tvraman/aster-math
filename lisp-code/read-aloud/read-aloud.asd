(in-package :asdf)

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
    :depends-on ("rule-and-style-macros"))
   (:file "time-readings" :depends-on  ("read-aloud"))
   (:file "handling-different-fonts")
   (:file "font-afl-state-definitions")
   (:file "handling-reading-states")
   (:file  "reading-state-definitions" :depends-on ("handling-reading-states" ))
   (:file "reading-styles-and-rules" )
   (:file "math-reader-aux")
   (:file "reading-rule-definitions"
    :depends-on ("reading-styles-and-rules"))
   (:file "summary-style"
    :depends-on (  "reading-styles-and-rules"))
   (:file "math-reading-rules"
    :depends-on ("reading-styles-and-rules" "math-reader-aux" ))
   (:file "descriptive-math-readings"
    :depends-on ("reading-styles-and-rules" "math-reader-aux" ))
   (:file "new-document-objects")
   (:file "standard-math-objects")
   (:file "literature")
   (:file "new-environment-definitions")
   (:file "complex-objects")
   (:file "special-pattern-macros")
   (:file "special-patterns"
    :depends-on ("special-pattern-macros"))

   (:file "float-macros")
   (:file "float-objects")
   (:file "float-readings")
   (:file "float-rules"
    :depends-on ("float-macros" "float-objects"
                                "float-readings"))
   (:file "var-subst")))
