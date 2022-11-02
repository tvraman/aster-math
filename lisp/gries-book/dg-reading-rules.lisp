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
   (read-aloud (argument 1 boxed-equation))
   )
  )

(activate-rule 'boxed-equation 'play-sound) 
