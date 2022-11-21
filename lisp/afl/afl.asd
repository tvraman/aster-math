(in-package :asdf)

(unless (find-package :afl)
  (make-package :afl :use '(:common-lisp :sb-ext )))

(defsystem "afl"
  :description "AFL: Audio Formatting Language"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   (:file "tts")
   (:file "structs")
   (:file "global-variables"
    :depends-on ("01-speech-space" "structs"))
   (:file "01-speech-space"
    :depends-on (  "tts" "05-block-structure" "structs"))
   (:file "02-user-definitions"
    :depends-on ("01-speech-space"))
   (:file "03-standard-voice-definitions"
    :depends-on ("structs"))
   (:file "04-moving-in-speech-space"
    :depends-on ("structs"))
   (:file "05-block-structure"
    :depends-on ("structs"))
   (:file "06-synthesizer-codes"
    :depends-on ("structs"))
   (:file "pronounce")
   (:file "pronunciations" :depends-on ("pronounce"))
   (:file "french-pronunciations" :depends-on ("pronounce" ))
   (:file  "interface-pronounce" :depends-on( "pronounce"))))
