(in-package :asdf)

(defsystem "total-space"
  :description "AFL: Total Space"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  ((:file "total-space-state" )
   (:file "initialize-total-space")
   (:file "assignments")))
