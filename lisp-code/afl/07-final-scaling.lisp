;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(export '(
          define-final-scale-factor
          set-final-scale-factor
          get-final-scale-factor
          refresh
          ))



;;; Implements a scaling operation to the current state.
;;; Allows for cleanly speeding up an entire rendering etc.
;;; Suggested by Jim Davis.

;;; This function uses a set of global scale factors initially set to
;;; unity.  They can be changed by the user to result in the entire
;;; reading being scaled. Will work with interruptions.

;;{{{ *table-of-final-scale-factors*

;;; Variable: *DEFAULT-FINAL-SCALE-FACTOR*                   Author: raman
;;; Created: Fri Aug 14 11:44:50 1992

(defvar *default-final-scale-factor* 1
  "Default scale factor along all dimensions in speech space")
;;; Variable: *TABLE-OF-FINAL-SCALE-FACTORS*                 Author: raman
;;; Created: Fri Aug 14 11:36:00 1992

(defvar *table-of-final-scale-factors*
  (make-hash-table   :test #'equal)
  " Table containing global scale factors")


;;; Function: DEFINE-FINAL-SCALE-FACTOR                      Author: raman
;;; Created: Fri Aug 14 11:37:43 1992

(defun define-final-scale-factor (dimension scale-factor) 
  "Define global scale factor to be applied to dimension Does nothing
if scale factor already defined "
  (unless (gethash dimension *table-of-final-scale-factors*) 
    (setf (gethash dimension *table-of-final-scale-factors*)
          scale-factor))
  )


;;; Function: GET-FINAL-SCALE-FACTOR                         Author: raman
;;; Created: Fri Aug 14 11:38:46 1992

(defun get-final-scale-factor (dimension) 
  "Retrieve global scale factor for dimension dimension default is unity"
  (or 
   (gethash dimension  *table-of-final-scale-factors*)
   *default-final-scale-factor*)
  )



;;; Function: SET-FINAL-SCALE-FACTOR                         Author: raman
;;; Created: Fri Aug 14 11:40:44 1992

(defun set-final-scale-factor (dimension scale-factor) 
  "Set scale factor for dimension"
  (setf (gethash dimension *table-of-final-scale-factors*)
        scale-factor)
  (with-lazy-set-state
      (set-speech-state *current-speech-state*))
  )

;;; Function: REFRESH                                        Author: raman
;;; Created: Thu Aug 20 14:04:49 1992

(defun refresh () 
  "Call this function if the hardware gets out of synch with the current
state as recorded by afl"
  (tts-queue "[:punc some]")
(with-lazy-set-state
  (set-speech-state *current-speech-state*))
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

