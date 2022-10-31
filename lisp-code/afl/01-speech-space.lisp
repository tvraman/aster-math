
;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

(export '(
          *default-voice*
          *reader-period-pause* *reader-comma-pause* 
          initialize-speech-space
          re-initialize-speech-space
          current-value
          ))

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

;;; The dimensions in the speech space are first captured in a list
;;; called *list-of-speech-dimensions* and this is used by all the functions.
;;; The structure definition for point-in-speech-space is
;;; generated from this list by a macro. 
;;;

;;}}}
;;{{{ dimension

;;;  Structure: DIMENSION                                     Author: raman
;;; Constant: *DEFAULT-VOICE*                                Author: raman
;;; Created: Sun Aug 30 19:11:20 1992
;;; external variable: 
(defvar *default-voice* 'paul  "default voice")



;;; note: the dimension voice is in a sense redundant. The other
;;; dimensions that occur in the speech space are independent of one
;;; another, they together determine the voice.
;;; Structure:  dimension Author: raman
;;; Created: Fri Aug  7 10:25:35 1992
;;; A dimension in speech space. 
(defstruct dimension
  (name nil)
  (value nil)
  (step-size nil) 
  )

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
   :step-size step-size 
   )
  )

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

(defmacro define-point-in-speech-space ()
  "define point in speech space  from dimensions defined so far"
  `(defstruct (point-in-speech-space :named (:type list))
     ,@ *list-of-speech-dimensions*))
; so the struct gets defined before it is used:

(define-point-in-speech-space)
;;; The following function relies on the list representation of the
;;; structure point-in-speech-space and will have to be changed if the
;;; structure representation for points in speech space is modified. 
;;; Function: UPDATE-POINT-IN-SPEECH-SPACE Author: raman
;;; Created: Sun Aug  9 18:04:02 1992

(defun update-point-in-speech-space (point dim-name dimension) 
  "set dimension for dim-name to dimension."
  (let
      ((index  (position dim-name *list-of-speech-dimensions* )))
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
         (position dimension  *list-of-speech-dimensions* )))
    (elt point (1+ index))
    )
  )
;;; Modified: Tue Aug 11 09:52:58 EDT 1992
;;; Made to work using *list-of-speech-dimensions*


;;; defsetf form for updating points
;;; Could be sued as an alternative to update-point-in-speech-space
;;; Also relies on the list representation of point-in-speech-space
;;; Usage:
;;; (setf (point-accessor dimension point) value) 

(defsetf  point-accessor (dimension point) (value)
  `(setf 
    (elt ,point 
     (1+ (position ,dimension *list-of-speech-dimensions* )))
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
        (dimension *list-of-speech-dimensions*)
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
        (dimension *list-of-speech-dimensions*)
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

(defun re-initialize-speech-space (&optional (voice *default-voice*))
  "reinitialize speech space with voice, the name of a point in speech
space."
  (declare (special *default-voice* ))
  (let
      ((new-voice (get-point-in-speech-space voice)))
    (dolist
        (dimension *list-of-speech-dimensions*)
      (set-global-value dimension
                        (current-value dimension new-voice))
      (set-step-size dimension
                     (current-step-size dimension new-voice))
      )
    (with-lazy-set-state
     (set-speech-state  *current-speech-state*)
     )
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



;;; Variable: *READER-PERIOD-PAUSE*                          Author: raman
;;; Created: Mon Apr 13 10:01:35 1992

(defparameter  *reader-period-pause*  -380  "period pause used by reader.")

;;; Variable: *READER-COMMA-PAUSE*                           Author: raman
;;; Created: Mon Apr 13 10:04:31 1992

(defparameter *reader-comma-pause*  -40  "comma pause used by reader.")


;;; Function: INITIALIZE-SPEECH-SPACE                        Author: raman
;;; Created: Fri Aug  7 12:44:56 1992
(defun initialize-speech-space (&optional(voice *default-voice*))
  "Initialize speech space by setting up current state and global state
based on the default settings specified for the various dimensions.
Default settings are overridden by settings specified by  the optional
argument to this function,  the name of a point in speech space"
  (assert  (gethash voice *standard-voices*) nil
           "error: Standard voice ~a not yet defined"
           voice)
  (setup-globals voice)
  (setf *speech-hardware-state* nil )
  (setf *current-speech-state* (create-initial-point-in-speech-space))
  (setf *global-speech-state* (create-initial-point-in-speech-space ))
  (tts-init)
  (set-speech-state *current-speech-state*)
  (set-period-pause *reader-period-pause*)
  (set-comma-pause *reader-comma-pause*))

;;; Function: SETUP-GLOBALS                                  Author: raman
;;; Created: Sat Aug  8 10:15:54 1992

(defun setup-globals (voice) 
  "setup global parameters appropriate to voice named voice."
  (let ((standard-voice (get-point-in-speech-space voice )))
    (dolist
        (dim *list-of-speech-dimensions*)
      (let
          ((dimension  (point-accessor dim standard-voice )))
        (when dimension 
          (define-globals dimension )
          )
        )
      )
    )
  )

;;; Function: DEFINE-GLOBALS                                 Author: raman
;;; Created: Sat Aug  8 11:10:29 1992

(defun define-globals (dimension) 
  "defines global values along dimension dimension as specified by the
argument which is an object of type dimension. "
  (assert  (dimension-p dimension) nil
           "Error: Argument to define-globals ~a is not of type dimension"
           dimension)
  (let
      ((name (dimension-name dimension )))
    (define-default-value name  (dimension-value dimension))
    (define-step-size name (dimension-step-size dimension))
    )
  )

;;}}}
;;{{{ rearrange-dimensions-for-dectalk

;;; This function resets *list-of-speech-dimensions* after rearranging the
;;; dimensions in the right order for dectalk.  The only thing that
;;; matters is that the voice if present be placed in front of the
;;; list.


;;}}}
