;;; Implement AFL Using Aural CSS (ACSS)

(in-package :acss)
;;; {01-speech-space:

(defstruct dimension
  (name nil)
  (value nil)
  (step-size nil))

(defun create-dimension (name &key value step-size )
  "return a dimension appropriate for dimension name"
  (make-dimension
   :name name
   :value value
   :step-size step-size))

(defvar *dimension-accessor-table*
  `(
    (value ,#'dimension-value
           ,#'(lambda (dimension  new-value)
                (setf (dimension-value  dimension) new-value)))
    (step-size ,#'dimension-step-size
               ,#'(lambda (dimension  new-step-size)
                    (setf (dimension-step-size  dimension) new-step-size ))))
  "Table of slot names and accessor functions for dimension structure.  ")

(defun dimension-accessor (slot dimension)
  "access slot slot of dimension dimension"
  (funcall (second (assoc slot  *dimension-accessor-table*)) dimension))

(defsetf dimension-accessor (slot dimension) (new-value)
  `(funcall (third (assoc ,slot *dimension-accessor-table*))
     ,dimension ,new-value))

(defvar *list-of-speech-dimensions*
  '(average-pitch pitch-range stress richness)
  "List of ACSS speech dimensions.")


(defmacro define-point-in-speech-space ()
  "define point in speech space  from dimensions defined so far"
  `(defstruct (point-in-speech-space
               (:named)
               (:type list))
     ,@ *list-of-speech-dimensions*))

(defun update-point-in-speech-space (point dim-name dimension)
  "set dimension for dim-name to dimension."
  (let ((index  (position dim-name *list-of-speech-dimensions* )))
    (setf (elt  point (1+ index)) dimension)
    point))

(defun point-accessor (dimension point)
  "Return value of slot dimension from point"
  (let ((index (1+  (position dimension  *list-of-speech-dimensions* ))))
    (elt point index)))

(defsetf  point-accessor (dimension point) (value)
  `(setf
     (elt ,point (1+ (position ,dimension *list-of-speech-dimensions* )))
     ,value))

(defun create-initial-point-in-speech-space ()
  "create a point in speech space with default settings that have
global scope"
  (let ((point (make-point-in-speech-space )))
    (dolist
        (dimension *list-of-speech-dimensions*)
      (update-point-in-speech-space point  dimension
                                    (make-dimension
                                     :name dimension
                                     :value (get-global-value dimension)
                                     :step-size (get-step-size dimension))))
    point))

(defun embed-point-in-speech-space (point)
  "Takes a point in speech space and returns a copy with the default
values assigned for those dimensions that are undefined in point"
  (let ((new-point (copy-point-in-speech-space point)))
    (dolist
        (dimension *list-of-speech-dimensions*)
      (unless  (point-accessor dimension new-point)
        (update-point-in-speech-space
         new-point dimension
         (create-dimension dimension ))))
    new-point))

(defun current-value (dimension &optional (point  *current-speech-state*))
  "Return current value assigned to dimension in this point. default
point is *current-speech-state* "
  (dimension-value (point-accessor   dimension point )))

(defun current-step-size (dimension &optional (point *current-speech-state*))
  "Return step size assigned to this dimension "
  (dimension-step-size  (point-accessor   dimension point )))

(defun re-initialize-speech-space ()
  "reinitialize speech space with voice, the name of a point in speech
space."
  (let ((new-voice (get-point-in-speech-space)))
    (dolist
        (dimension *list-of-speech-dimensions*)
      (set-global-value dimension (current-value dimension new-voice))
      (set-step-size dimension (current-step-size dimension new-voice)))
    (with-lazy-set-state (set-speech-state  *current-speech-state*))))

(defun initialize-speech-space (&optional(voice 'paul))
  "Initialize speech space by setting up current state and global state
based on the default settings specified for the various dimensions.
Default settings are overridden by settings specified by  the optional
argument to this function,  the name of a point in speech space"
  (setup-globals voice)
  (setf *current-speech-state* (create-initial-point-in-speech-space))
  (setf *global-speech-state* (create-initial-point-in-speech-space ))
  (set-speech-state *current-speech-state*))

;;; }
