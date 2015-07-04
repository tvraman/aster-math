(in-package :asdf)

(unless (find-package :afl) (make-package :afl :use '(:tts)))

(defsystem "afl"
  :description "AFL: Audio Formatting Language"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  (
   (:file "reference-variables")
   (:file "global-variables")
   (:file "01-speech-space"
          :depends-on
          ( "reference-variables"
            "global-variables"
            "05-block-structure"))
   (:file "02-user-definitions"
          :depends-on
          ( "reference-variables"
            "global-variables"
            "01-speech-space"
            "07-final-scaling"))
   (:file "03-standard-voice-definitions")
   (:file "04-moving-in-speech-space")
   (:file "05-block-structure" )
   (:file "06-synthesizer-codes")
   (:file "dectalk-specific-code")
   (:file "07-final-scaling")
   (:file "total-space/total-space-state" )
   (:module "pronounce"
            :components
            (
             (:file "pronounce")
             (:file "pronunciations" :depends-on ("pronounce"))
             (:file "french-pronunciations" :depends-on ("pronounce" ))
             (:file  "interface-pronounce" :depends-on( "pronounce"))))
   (:module "total-space"
            :components
            ((:file "total-space-state")
             (:file "initialize-total-space"
                    :depends-on ("total-space-state"))
             (:file "assignments"))
            :depends-on ( "pronounce" )))
  :depends-on ( tts))
