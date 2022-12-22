;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)

;;; Mon Oct 25 09:33:03 EDT 1993
 ;;; Defines some floating reading rules.
;;; Uses delay-until. Relies on after methods on read-aloud to force
;;; the delay-objects at the appropriate time.
;;;
(define-reading-state 'footnote-mark
    #'(lambda(state)
        (declare (ignore state))
        (afl:step-by afl:*current-speech-state* 'afl:average-pitch 2 )))

(def-reading-rule (footnote float)
  "Make footnotes float. "
  (let ((counter (counter-value 'footnote )))
    (afl:new-block
      (with-reading-state (reading-state 'footnote-mark)
        (afl:tts-queue  (format nil " ~a " counter ))))
    (increment-counter-value 'footnote)
    (delay-until 'paragraph
                 #'(lambda()
                     (without-float (footnote)
                       (decrement-counter-value 'footnote)
                       (afl:new-block
                         (with-reading-state
                             (reading-state 'annotation-voice)
                           (read-aloud
                            (format nil
                                    "footnote ~a," counter )))
                         (read-aloud footnote )))))))

(def-reading-rule (fraction float)
  "float fractions to the end of math. "
  (unless (find 'math *can-this-have-floats* )
    (push 'math *can-this-have-floats*))
  (read-aloud "fraction" )
  (delay-until  'math
                #'(lambda()
                    (without-float (fraction)
                      (read-aloud fraction ))))
  )
