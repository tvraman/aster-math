
;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :afl)

(export '(refresh))

;;; Implements a scaling operation to the current state.
;;; Allows for cleanly speeding up an entire rendering etc.
;;; Suggested by Jim Davis.

;;; This function uses a set of global scale factors initially set to
;;; unity.  They can be changed by the user to result in the entire
;;; reading being scaled. Will work with interruptions.

;;{{{ *table-of-final-scale-factors*

;;; Function: REFRESH                                        Author: raman
;;; Created: Thu Aug 20 14:04:49 1992

(defun refresh ()
  "Call this function if the hardware gets out of synch with the current
state as recorded by afl"
  (tts-queue "[:punc some]")
  (set-speech-state *current-speech-state*)
  )

;;}}}

;;; Relies on the list representation of point-in-speech-space
;;; Function: SCALE-POINT-IN-SPEECH-SPACE                    Author: raman
;;; Created: Fri Aug 14 11:47:08 1992

(defun scale-point-in-speech-space (point)
  "Apply scaling to a point in speech space"
  (let
      ((new-point (copy-point-in-speech-space  point )))
    (dolist
        (dimension (rest new-point))
      (when (dimension-p dimension )
;;; scale only if factor not 'undefined
        (unless   (equal  'undefined (get-final-scale-factor
                                      (dimension-name dimension )))
          (update-point-in-speech-space new-point
                                        (dimension-name dimension)
                                        (scale-dimension dimension) )
          ))
      )
    new-point)
  )

;;; Function: SCALE-DIMENSION                                Author: raman
;;; Created: Fri Aug 14 11:51:21 1992

(defun scale-dimension  (dimension)
  "Return a scaled copy of dimension"
  (let
      ((new-dimension (copy-dimension dimension )))
    (setf (dimension-value new-dimension)
          (*  (reference-value  (dimension-value new-dimension))
              (get-final-scale-factor (dimension-name dimension ))))
    new-dimension
    )
  )
