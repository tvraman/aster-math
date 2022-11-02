(defsystem "pronounce"
  :description "Pronunciation component for AFL"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   (:file "pronounce")
   (:file "pronunciations"
          :depends-on ("pronounce"))
   (:file "french-pronunciations"
          :depends-on ("pronounce" ))
   (:file  "interface-pronounce"
           :depends-on( "pronounce"))))
