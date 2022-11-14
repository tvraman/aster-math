;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;; Contains reading rule definitions  for objects from tcs chicago journal

(defmethod read-aloud ((end-of-sentence-mark end-of-sentence-mark ))
  nil)

(defmethod end-of-sentence? ((end-of-sentence end-of-sentence ))
  "Always ends a sentence. "
  t )

(defmethod read-aloud ((asectional-unit asectional-unit))
  "read absolute sectional-unit"
  (with-reading-state (reading-state 'annotation-voice)
    (read-aloud (sectional-unit-name asectional-unit))
    (when (absolute-number  asectional-unit)
      (read-aloud (absolute-number asectional-unit ))))
  (with-reading-state (reading-state 'title-voice)
    (read-aloud (sectional-unit-title asectional-unit )))
  (when (sectional-unit-body asectional-unit)
    (afl:new-block
      (read-aloud  (sectional-unit-body asectional-unit)))
    (afl:tts-force))
  (afl:new-block
    (read-aloud (sectional-unit-sectional-units asectional-unit ))))
