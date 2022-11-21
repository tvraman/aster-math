;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)


;;; Mon Dec 21 09:25:41 EST 1992
;;; Reading rules for objects defined in <(object definitions for Zippel)>

(defmethod read-aloud  (( keyi keyi ))
  "Read aloud method for object keyi "
  (read-aloud  (argument keyi 1)))

(defmethod read-aloud  (( addsymbol addsymbol ))
  "Read aloud method for object addsymbol "
  (read-aloud (argument addsymbol 1))
  (afl:tts-queue  "denotes ")
  (read-aloud (argument addsymbol 2 )))
