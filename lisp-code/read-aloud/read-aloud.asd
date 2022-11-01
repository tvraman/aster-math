(unless (find-package :read-aloud) (make-package :read-aloud))

(asdf:defsystem "read-aloud"
  :description "AsTeR's Audio Formatting Rules"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components  
  ((:module "cardinal"
    :pathname ""
    :components ((:file "cardinal-numbers" )))
   (:module "user-input"
    :pathname ""
    :components ((:file "get-user-feedback" )))
   (:module "read-aloud"
    :pathname ""
    :components
    ((:file "rule-and-style-macros")
     (:file "read-aloud"
      :depends-on ("rule-and-style-macros"))
     )
    :depends-on ( "font-definitions" "user-input" "reading-state"))
   (:module  "font-definitions"
    :pathname ""
    :components 
    ((:file "handling-different-fonts")
     (:file "font-afl-state-definitions"
      :depends-on ("handling-different-fonts" ))))
   (:module "reading-state"
    :pathname "" 
    :components 
    ((:file "handling-reading-states")
     (:file  "reading-state-definitions"
      :depends-on ("handling-reading-states" ))))
   (:module "reading-rules-and-styles"
    :pathname "" 
    :components 
    ((:file "reading-styles-and-rules" )
     (:file "math-reader-aux")
     (:file "reading-rule-definitions"
      :depends-on (                                      "reading-styles-and-rules"))
     (:file "summary-style"
      :depends-on (  "reading-styles-and-rules"))
     (:file "math-reading-rules"
      :depends-on ("reading-styles-and-rules" "math-reader-aux" ))
     (:file "descriptive-math-readings"
      :depends-on ("reading-styles-and-rules" "math-reader-aux" )))
    :depends-on ("new-document-objects" "complexity"))
   (:module "new-document-objects"
    :pathname ""
    :components
    ((:file "new-document-objects")
     (:file "standard-math-objects")
     (:file "new-environment-definitions")))
   (:module "complexity"
    :pathname ""
    :components
    ((:file "complex-objects"))
    :depends-on ("new-document-objects"))
   (:module "special-patterns"
    :pathname ""
    :components 
    ((:file "special-pattern-macros")
     (:file "special-patterns"
      :depends-on ("special-pattern-macros")))
    :depends-on ("read-aloud" "complexity"
                 "reading-rules-and-styles" "reading-state" "new-document-objects"))
   (:module "floats"
    :pathname ""
    :components 
    ((:file "float-macros")
     (:file "float-objects")
     (:file "float-readings")
     (:file "float-rules"
      :depends-on ("float-macros" "float-objects" "float-readings"))))
   (:module "var-subst"
    :pathname ""
    :components 
    ((:file "var-subst"))
    :depends-on
    ("read-aloud"
     "complexity"
     "reading-rules-and-styles" "reading-state" "new-document-objects"))))
