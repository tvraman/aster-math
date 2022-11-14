;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(def-reading-rule (boxed-equation play-sound)
  "Read aloud method for object boxed-equation "
  (afl:new-block
    (if (label boxed-equation)
        (read-aloud (label-name (label boxed-equation )))
        (read-aloud (format nil "~a  ~a. "
                            (contents boxed-equation)
                            (anumber boxed-equation ))))
    (afl:tts-pause 5)
    (read-aloud (argument boxed-equation 1))))

(activate-rule 'boxed-equation 'play-sound)

(defmethod read-aloud ((constraint-brackets constraint-brackets))
  (with-reading-state (reading-state 'subscript )
    (read-aloud (children constraint-brackets ))))

(defmethod read-aloud  (( defined-text-object-with-label defined-text-object-with-label ))
  "Read aloud method for object defined-text-object-with-label "
  (afl:new-block
    (if (label defined-text-object-with-label)
        (read-aloud (label-name (label defined-text-object-with-label )))
        (read-aloud (format nil "~a  ~a. "
                            (contents defined-text-object-with-label)
                            (anumber defined-text-object-with-label ))))
    (afl:tts-pause 5)
    (read-aloud (argument defined-text-object-with-label 1))
    (relabel-if-necessary (label defined-text-object-with-label ))))

(defmethod read-aloud  (( lefteqn lefteqn ))
  "Read aloud method for object lefteqn "
  (read-aloud (argument lefteqn 1 )))
