;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))

(defmethod read-aloud  (( kd-e kd-e ))
  "Read aloud method for object kd-e "
  nil)

(defmethod read-aloud  (( snoindent snoindent ))
  "Read aloud method for object snoindent "
  nil)

(defmethod read-aloud  (( pno pno ))
  "Read aloud method for object pno "
  (read-aloud "end of page ")
  (read-aloud (argument pno 1))
  (afl:tts-queue  "[_.]")
  (afl:tts-icon *newline-cue*))

(defmethod read-aloud  (( seject seject ))
  "Read aloud method for object seject "
  nil)

(defmethod read-aloud  (( kd-h kd-h ))
  "Read aloud method for object kd-h "
  (afl:tts-queue "[_.]"))

(defmethod read-aloud  (( fontbi fontbi ))
  "Read aloud method for object fontbi "
  nil)

(defmethod read-aloud  (( leqno leqno ))
  "Read aloud method for object leqno "
  (read-aloud "equation "))
