
;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :afl)

;;; Implements a scaling operation to the current state.
;;; Allows for cleanly speeding up an entire rendering etc.
;;{{{ *table-of-final-scale-factors*

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
