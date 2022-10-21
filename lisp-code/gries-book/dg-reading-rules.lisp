;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)


(def-reading-rule (boxed-equation play-sound) 
    "Read aloud method for object boxed-equation "
  (afl:new-block
   (if (label boxed-equation)
       (read-aloud (label-name (label boxed-equation )))
       (read-aloud (format nil "~a  ~a. "
                           (contents boxed-equation)
                           (number boxed-equation ))))
   (afl:pause 5) 
   (read-aloud (argument 1 boxed-equation))
   )
  )

(activate-rule 'boxed-equation 'play-sound) 
