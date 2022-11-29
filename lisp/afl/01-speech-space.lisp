;;;   -*-   Mode: LISP -*-    ;;;


;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)


(export '(
          initialize-speech-space re-initialize-speech-space current-value))

;;{{{ method reference-value
(defgeneric reference-value (ref))

;;; Overloading reference-value. If argument is not a reference
;;; object, just return it, otherwise return the slot value.
;;; Method: REFERENCE-VALUE                                  Author: raman
;;; Created: Fri Aug  7 11:44:54 1992
(defmethod reference-value ((ordinary t))
  "return argument"
  ordinary)

;;; Method: REFERENCE-VALUE                                  Author: raman
;;; Created: Fri Aug  7 11:48:37 1992
(defmethod reference-value ((reference reference))
  "return value of ref"
  (reference-val reference))

;;}}}

;;; Each fold contains functions associated with the fold marker
;;{{{ Introduction:

;;; This file contains definitions of points in the speech space.
;;; Points in the speech space are currently implemented as
;;; structures. A point in the speech space determines the parameter
;;; values along the various dimensions defining the space. Dimensions
;;; include head-size, breathiness, voice, volume etc.  The information
;;; pertinent to each dimension is captured in a structure called
;;; dimension.  Relevant information includes name of the dimension,
;;; value, default step size.
;;; the structure dimension captures those bits of information that
;;; are liable to change, eg: value, step-size. Synthesizer code will
;;; not change for a dimension during the program, so this is kept
;;; separately in a hash table.
;;;
;;; The point in speech space will contain fields each of which will
;;; be assigned an object of type dimension encapsulating the current
;;; settings along that dimension.
;;; The structure for points in speech space is automatically
;;; generated from the list of dimensions specified by the user.
;;; Global values are stored in a special structure that implements
;;; reference variables.

;;}}}
;;{{{Dimension:

;;;

;;; function create-dimension takes a name and creates a structure of
;;; type dimension with the values set to their initial defaults. It
;;; does this by doing a table look up for the various fields.
;;; Modified: Tue Aug 18 14:41:37 EDT 1992
;;; Dereference global values if used as defaults when creating a new
;;; dimension.
;;; Function: CREATE-DIMENSION                               Author: raman
;;; Created: Fri Aug  7 18:10:21 1992

(defun create-dimension (name
                         &key(value(reference-value  (get-global-value name)))
                           (step-size (reference-value  (get-step-size name ))))
  "return a dimension appropriate for dimension name"
  (make-dimension
   :name name
   :value value
   :step-size step-size))

;;; Adding accessor table to allow for setf form for
;;; dimension-accessor.  This is because I need to modify slots in the
;;; dimension structure in the move operators by supplying a slot
;;; name.

;;; Variable: *DIMENSION-ACCESSOR-TABLE*                     Author: raman
;;; Created: Wed Aug 19 09:02:51 1992

(defvar *dimension-accessor-table*
  `(
    (value ,#'dimension-value
           ,#'(lambda (dimension  new-value)
                (setf (dimension-value  dimension) new-value)))
    (step-size ,#'dimension-step-size
               ,#'(lambda (dimension  new-step-size)
                    (setf (dimension-step-size  dimension) new-step-size )))
    )
  "Table of slot names and accessor functions for dimension structure.  ")

;;; Function: DIMENSION-ACCESSOR                             Author: raman
;;; Created: Wed Aug 19 09:09:20 1992

(defun dimension-accessor (slot dimension)
  "access slot slot of dimension dimension"
  (funcall (second (assoc slot  *dimension-accessor-table*)) dimension)
  )

;;; DEFSETF form for modifying slots in dimension:

(defsetf dimension-accessor (slot dimension) (new-value)
  `(funcall (third (assoc ,slot *dimension-accessor-table*))
     ,dimension ,new-value)
  )

;;}}}
;;{{{ point-in-speech-space

;;; point-in-speech-space a structure:
;;; this is  generated automatically from the list of
;;; dimensions.
;;; point in speech space

;;; Function: UPDATE-POINT-IN-SPEECH-SPACE Author: raman
;;; Created: Sun Aug  9 18:04:02 1992

(defun update-point-in-speech-space (point dim-name dimension)
  "set dimension for dim-name to dimension."
  (let
      ((index  (position dim-name *speech-dimensions* )))
    (setf (elt  point (1+ index))
          dimension)
    point)
  )
;;; Following function relies on the
;;; list representation of point-in-speech-space
;;; Function: POINT-ACCESSOR                                 Author: raman
;;; Created: Sat Aug 15 09:10:28 1992

(defun point-accessor (dimension point)
  "Return value of slot dimension from point"
  (let
      ((index
                                        ;1+
         (position dimension  (speech-dimensions) )))
    (elt point (1+ index))
    )
  )
;;; Modified: Tue Aug 11 09:52:58 EDT 1992

;;; defsetf form for updating points
;;; Could be sued as an alternative to update-point-in-speech-space
;;; Also relies on the list representation of point-in-speech-space
;;; Usage:
;;; (setf (point-accessor dimension point) value)

(defsetf  point-accessor (dimension point) (value)
  `(setf
     (elt ,point
       (1+ (position ,dimension (speech-dimensions) )))
     ,value))

;;; Modified: Tue Aug 18 15:55:04 EDT 1992
;;; creates a point in the speech space at global scope, ie: global
;;; sets will affect the settings of this point. To be used by the
;;; function that initializes the speech space.

;;; Function: CREATE-initial-POINT-IN-SPEECH-SPACE                   Author: raman
;;; Created: Fri Aug  7 18:18:07 1992

(defun create-initial-point-in-speech-space ()
  "create a point in speech space with default settings that have
global scope"
  (let ((point (make-point-in-speech-space )))
    (dolist
        (dimension (speech-dimensions))
      (update-point-in-speech-space point  dimension
                                    (make-dimension
                                     :name dimension
                                     :value (get-global-value
                                             dimension)
                                     :step-size (get-step-size dimension)
                                     )
                                    )
      )
    point)
  )

;;; Function: EMBED-POINT-IN-SPEECH-SPACE                Author: raman
;;; Created: Wed Aug 12 10:24:38 1992

(defun embed-point-in-speech-space (point)
  "Takes a point in speech space and returns a copy with the default
values assigned for those dimensions that are undefined in point"
  (let
      ((new-point (copy-point-in-speech-space point)))
    (dolist
        (dimension (speech-dimensions))
      (unless  (point-accessor dimension new-point)
        (update-point-in-speech-space new-point dimension
                                      (create-dimension
                                       dimension ))
        ))
    new-point)
  )

;;; Function: CURRENT-VALUE                                  Author: raman
;;; Created: Mon Aug 24 09:54:46 1992

(defun current-value (dimension &optional (point  *current-speech-state*) )
  "Return current value assigned to dimension in this point. default
point is *current-speech-state* "
  (reference-value (dimension-value (point-accessor   dimension point )))
  )

;;; Function: CURRENT-STEP-SIZE                                  Author: raman
;;; Created: Wed Aug 26 15:36:30 1992

(defun current-step-size (dimension &optional (point *current-speech-state*))
  "Return step size assigned to this dimension "
  (reference-value (dimension-step-size  (point-accessor   dimension point )))

  )

;;; Function: RE-INITIALIZE-SPEECH-SPACE                     Author: raman
;;; Created: Wed Aug 26 15:29:23 1992

(defun re-initialize-speech-space (&optional (voice 'paul))
  "re-initialize speech space with voice,  a point in speech space."
  (let ((new-voice (get-point-in-speech-space voice)))
    (dolist
        (dimension (speech-dimensions))
      (set-global-value dimension
                        (current-value dimension new-voice))
      (set-step-size dimension
                     (current-step-size dimension new-voice))
      )
    (set-speech-state  *current-speech-state*)

    )
  )

;;}}}
;;{{{ initialize-speech-space

;;; comments on initializing speech space:
;;; initializing speech space will include the following steps:
;;; Do a table lookup and get the values for the default voice
;;; specified by the user. Store associated values in the
;;; *global-values* so that global-set can manipulate it. Construct
;;; a point in speech space which is the origin and set current-state
;;; to this point.
;;;
;;; How this will be implemented:
;;; initialize-speech-space takes an optional argument a voice-name.
;;; It then looks up the default values for this voice. These are then
;;; entered into the various tables. Thus when this function finishes
;;; its work, we have the default settings as well as a start point.
;;; Function: INITIALIZE-SPEECH-SPACE                        Author: raman
;;; Created: Fri Aug  7 12:44:56 1992
(defun initialize-speech-space (&optional(voice 'paul))
  "Initialize speech space by setting up current state and global state
based on the default settings specified for the various dimensions.
Default settings are overridden by settings specified by  the optional
argument to this function,  the name of a point in speech space"
  (assert  (gethash voice (standard-voices)) nil
           "error: Standard voice ~a not yet defined"
           voice)
  (setup-globals voice)
  (setf *speech-hardware-state* nil )
  (setf *current-speech-state* (create-initial-point-in-speech-space))
  (setf *global-speech-state* (create-initial-point-in-speech-space ))
  ;(tts-init)
  (set-speech-state *current-speech-state*))

;;; Function: SETUP-GLOBALS                                  Author: raman
;;; Created: Sat Aug  8 10:15:54 1992

(defun setup-globals (voice)
  "setup global parameters appropriate to voice named voice."
  (let ((standard-voice (get-point-in-speech-space voice )))
    (dolist
        (dim (speech-dimensions))
      (let
          ((dimension  (point-accessor dim standard-voice )))
        (when dimension
          (define-globals dimension ))))))

;;; Function: DEFINE-GLOBALS                                 Author: raman
;;; Created: Sat Aug  8 11:10:29 1992

(defun define-globals (dimension)
  "defines global values along dimension dimension as specified by the
argument which is an object of type dimension. "
  (assert
   (dimension-p dimension) nil
   "Error: Argument to define-globals ~a is not of type dimension" dimension)
  (let ((name (dimension-name dimension )))
    (define-default-value name  (dimension-value dimension))
    (define-step-size name (dimension-step-size dimension))))

;;}}}
;;; Include 07-final-scaling here

(export
 '(define-final-scale-factor
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
   *default-final-scale-factor*))




;;; Function: SET-FINAL-SCALE-FACTOR                         Author: raman
;;; Created: Fri Aug 14 11:40:44 1992

(defun set-final-scale-factor (dimension scale-factor)
  "Set scale factor for dimension"
  (setf (gethash dimension *table-of-final-scale-factors*)
        scale-factor)
  (set-speech-state *current-speech-state*)
  )


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
  (let ((new-dimension (copy-dimension dimension )))
    (setf (dimension-value new-dimension)
          (*  (reference-value  (dimension-value new-dimension))
              (get-final-scale-factor (dimension-name dimension ))))
    new-dimension
    )
  )
