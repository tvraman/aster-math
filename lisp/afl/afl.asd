(in-package :asdf)

(unless (find-package :afl)
  (make-package :afl :use '(:cl :common-lisp )))

(defsystem "afl"
  :description "AFL: Audio Formatting Language"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   (:file "tts")
   (:file "global-variables")
   (:file "01-speech-space"
    :depends-on ( "global-variables" "tts" "05-block-structure"))
   (:file "02-user-definitions"
    :depends-on ("01-speech-space"))
   (:file "03-standard-voice-definitions")
   (:file "04-moving-in-speech-space")
   (:file "05-block-structure" )
   (:file "06-synthesizer-codes")))
