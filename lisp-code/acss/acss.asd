(in-package :asdf)


  (defpackage :acss
    (:use :common-lisp))

(defsystem "acss"
  :description "AFL: Audio Formatting Language Using ACSS"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  ((:file "acss.lisp")))
