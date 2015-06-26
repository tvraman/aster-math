(in-package :asdf)
(unless (find-package 'tts)
  (make-package 'tts :use '(cl)))

(defsystem "tts"
  :description "TTS: Connect Lisp to Emacspeak Speech Server"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com"
  :licence "GPL V2"
  :components ((:file "tts")))

