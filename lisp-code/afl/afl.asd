(in-package :asdf)

(defsystem "afl"
  :description "AFL: Audio Formatting Language"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"

  (unless (find-package 'afl)
    (make-package 'afl :use '(lisp    tts pcl  make)))


  

;;; Variable: *AFL-PATHNAME*                                 Author: raman
;;; Created: Fri Aug  7 17:34:12 1992

  (defvar *afl-pathname*
    (concatenate 'string *lisp-code-directory*  "/" "afl")
    "source code resides here")



;;; At present need to recompile the speech space method for
;;; local-set-state   after loading the total space.
;;; This is because the local-set-state method for component spaces
;;; needs to refer to the structure accessors defined in total-space.

;;; Modified: Tue Mar 23 12:31:30 EST 1993
;;; adding pronounce module here.
;;; pronounce module used to be in read-aloud.
                                        ;(setq *announce* nil)
  (defsystem afl
    :source-pathname *afl-pathname*
    :package afl
    :initially-do (print  "loading afl")
    :finally-do (progn  (print  "done loading afl")
                        (pushnew :afl *features*)
                        )
    :components
    (
     (:module "total-space-structure"
              :source-pathname "total-space"
              :components
              (
               (:file "total-space-state" )))
     (:module "pronounce"
              :source-pathname "pronounce"
              :components
              (
               (:file "pronounce")
               (:file "pronunciations" :depends-on ("pronounce"))
               (:file "french-pronunciations" :depends-on ("pronounce" ))
               (:file  "interface-pronounce" :depends-on( "pronounce"))
               )
              :depends-on ("speech-component"))
     (:module "speech-component"
              :source-pathname ""
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
               )
              )
     (:module "total-space"
              :source-pathname "total-space"
              :components
              ((:file "total-space-state")
               (:file "initialize-total-space"
                      :depends-on ("total-space-state"))
               (:file "assignments"))
              :depends-on ( "pronounce" "speech-component")))
    :depends-on ( tts))
