(in-package :asdf)

(unless (find-package :aster)
  (make-package :aster :use '(:cl :common-lisp )))

(defsystem "aster"
  :description "ASTER: Audio System For Technical Readings"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components ((:file "aster")))
