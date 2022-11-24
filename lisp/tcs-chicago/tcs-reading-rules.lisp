;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
 

(in-package :aster)

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
    (read-aloud (title asectional-unit )))
  (when (body asectional-unit)
    (afl:new-block
      (read-aloud  (body asectional-unit)))
    (afl:tts-force))
  (afl:new-block

    (read-aloud (children asectional-unit ))))
