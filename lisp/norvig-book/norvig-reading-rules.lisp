;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :cl-user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))

(def-reading-rule (asis  asis-skip)
    "interactively skip over code in norvig's book"
  (read-aloud "Want to read code? ")
  (when (yes-or-no-p "want to read code? ")
    (afl:with-pronunciation-mode (:mode :lisp)
    (read-aloud (contents asis ))))
  )

 

(defmethod read-aloud  (( idx-term idx-term )) 
  "Read aloud method for object idx-term "
  (with-reading-state (reading-state 'emphasize)
(read-aloud (argument idx-term 1 ))))
(defmethod read-aloud ((asis asis))
  "Read out contents of asis"
  (afl:with-pronunciation-mode (:mode :lisp)
    (read-aloud (contents asis ))))
(defmethod read-aloud  (( askip askip )) 
  "Read aloud method for object askip "
  nil)
(defmethod read-aloud  (( norvig-idx norvig-idx )) 
  "Read aloud method for object norvig-idx "
  nil)
