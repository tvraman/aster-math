(unless (find-package 'tts)
  (make-package 'tts :use '(cl)))
(in-package :asdf)

(defsystem "tts"
  :description "TTS: Connect Lisp to Emacspeak Speech Server"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com"
  :licence "GPL V2"
  :components ((:file "tts")))

