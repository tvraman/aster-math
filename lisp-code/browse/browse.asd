
;;; browser:
;;; Tue Jan 12 16:27:09 EST 1993
;;; Also load reader-browse.el into emacs:
(in-package :asdf)

(defsystem "browse"
  :description "AsTeR Browser"
  :version "1.0"
  :author "T. V. Raman <tv.raman.tv@gmail.com>"
  :licence "GPL V2"
  :components
  ((:module "browse-macros"
            :pathname ""
            :components ((:file "browse-macros" )))
   (:module "cross-reference"
            :pathname ""
            :components ((:file "cross-references"))
            :depends-on ("browse-macros"))
   (:module "browser-action"
            :pathname ""
            :components 
            ((:file "summarize")
             (:file "move-around")
             (:file "bookmark"))
            :depends-on ("browse-macros"))))
