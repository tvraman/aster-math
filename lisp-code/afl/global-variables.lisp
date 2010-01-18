;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))



(export '(
          add-dimension
          define-default-value
          define-unit-size
          define-synthesizer-code
          define-step-size
          set-step-size
          define-standard-voice
          save-point-in-speech-space
          get-point-in-speech-space
          list-of-speech-dimensions
          get-step-size
          define-minimum-value
          define-maximum-value
          minimum-value
          maximum-value
          dimension-range
          compute-range
          length-of-subinterval
          ))

;;; Each fold  contains a variable and its associated accessors and
;;; modifiers.
;;; Only the modifiers exported. 
;;; Contains all defvars etc used by afl.
;;; Also contains accessor and modifiers for these defvars.

;;; { *list-of-speech-dimensions*

;;; Variable: *LIST-OF-SPEECH-DIMENSIONS*                           Author: raman
;;; Created: Sat Aug  8 15:35:18 1992

(defvar *list-of-speech-dimensions*
  nil
  "list of dimension names that make up the speech space. ")

;;; This function pushes the name on to the the global list of
;;; dimensions, and will later also set up the global values etc. Will
;;; possibly generate a defstruct for defining the point in speech
;;; space. Hold this off until second pass.
;;; Modified: Tue Aug 18 12:20:31 EDT 1992

;;; added call to rearrange-dimensions-for-dectalk inside this
;;; function.
;;; Function: ADD-DIMENSION                                  Author: raman
;;; Created: Sat Aug  8 15:45:51 1992

(defun add-dimension (name) 
  "Add dimension named NAME to speech space"
  (push name *list-of-speech-dimensions*)
  (rearrange-dimensions-for-dectalk)
  )


;;; Function: LIST-OF-SPEECH-DIMENSIONS                             Author: raman
;;; Created: Sat Oct  3 12:18:18 1992

(defun list-of-speech-dimensions () 
  "Return current list of dimensions"
  *list-of-speech-dimensions*
  )
;;; }
;;; { *global-values*

;;;  comments on global settings implementation

;;; *global-values* holds the global settings of parameters in the
;;; speech space.  It is a hashtable, and has an 
;;; entry  associated with each dimension in the speech space,
;;; and the value assigned to it is of type reference, a structure
;;; that implements indirection.
;;; global will be automatically defined by the user defining
;;; the speech space. Provide set of macros for defining speech space
;;; abstractly once the first pass of the afl-language is done.
;;; The function setup-globals sets up global settings base don the
;;; dimensions defined. etc. 
;;; currently global settings are stored in a hash table.

;;; 

;;; Variable: *GLOBAL-VALUES*                              Author: raman
;;; Created: Fri Aug  7 12:46:20 1992
(defvar *global-values* (make-hash-table :test #'equal  )
  "global parameter settings")


;;;  Function: DEFINE-DEFAULT-VALUE Author: raman
;;; Created: Fri Aug  7 12:49:30 1992
(defun  define-default-value (dimension value)
  "Assign VALUE as the default value along dimension DIMENSION."
  (setf (gethash dimension *global-values* )
        (make-reference :val  value))
  )



;;; Function: GET-GLOBAL-VALUE                                  Author: raman

;;; Created: Fri Aug  7 13:29:55 1992
(defun  get-global-value  (dimension) 
  "return global value for dimension dimension."
  (gethash dimension *global-values*)
  )


;;; Function: SET-GLOBAL-VALUE                               Author: raman
;;; Created: Wed Aug 26 15:18:24 1992

(defun set-global-value (dimension value) 
  "set new global value to set for the first time use define-default-value"
  (assert (gethash dimension *global-values*) nil
          "Error: First define a global value for dimension ~a "
          dimension)
  (setf (reference-val (gethash dimension *global-values*) )
        value)
  )

;;; }
;;; { *table-of-units*

;;; implements tables for holding  information about speech space and
;;; how to manipulate it.
;;; For each slot in the structure point-in-speech-space there is a
;;; table that holds the dimension value pair for that slot. For the
;;; value slot, this is the same as the *global-values* defined in
;;; 01-speech-space.lisp  The tables implemented in this file need not
;;; to be known to global-set, which is why they reside here.

;;; in addition unit-size and synthesizer-code which are not expected
;;; to change during a program are also kept in tables here, and their
;;; associated fields do not appear in the structure dimension.

;;; All tables are implemented as hash tables for the present.  May
;;; change later:

;;; Variable: *TABLE-OF-UNITS*                               Author: raman
;;; Created: Fri Aug  7 17:49:29 1992

(defvar *table-of-units*
  (make-hash-table :test #'equal  )
  "table containing units for each dimension")

;;; Function: DEFINE-UNIT-SIZE                               Author: raman
;;; Created: Fri Aug  7 17:51:13 1992

(defun define-unit-size (dimension value) 
  "Define  VALUE as unit-size for dimension DIMENSION"
  (setf (gethash dimension *table-of-units*)
        value)
  )

;;; Function: GET-UNIT-SIZE                                  Author: raman
;;; Created: Fri Aug  7 17:52:43 1992

(defun get-unit-size (dimension) 
  "retrieve unit for this dimension"
  (gethash dimension *table-of-units*)
  )

;;; }
;;; {*table-of-synthesizer-codes*

;;; Variable: *TABLE-OF-SYNTHESIZER-CODES* Author: raman
;;; Created: Fri Aug  7 17:49:29 1992

(defvar *table-of-synthesizer-codes*
  (make-hash-table :test #'equal  )
  "table containing synthesizer codes  for each dimension")

;;; Function: DEFINE-SYNTHESIZER-CODE Author: raman
;;; Created: Fri Aug  7 17:51:13 1992

(defun define-synthesizer-code (dimension value) 
  "Define   VALUE as the synthesizer-code for setting dimension DIMENSION"
  (setf (gethash dimension *table-of-synthesizer-codes*)
        value)
  )

;;; Function: GET-SYNTHESIZER-CODE Author: raman
;;; Created: Fri Aug  7 17:52:43 1992

(defun get-synthesizer-code (dimension) 
  "retrieve synthesizer code  for this dimension"
  (gethash dimension *table-of-synthesizer-codes*)
  )

;;; }
;;; { *table-of-step-sizes*

;;; Variable: *TABLE-OF-STEP-SIZES* Author: raman
;;; Created: Fri Aug  7 17:49:29 1992

(defvar *table-of-step-sizes*
  (make-hash-table :test #'equal  )
  "table containing step sizes   for each dimension")
;;; Modified: Fri Aug 21 09:00:00 EDT 1992
;;; step sizes also stored as reference variables.

;;; Function: DEFINE-STEP-SIZE                               Author: raman
;;; Created: Fri Aug  7 17:51:13 1992

(defun define-step-size (dimension value) 
  "Define VALUE as the step-size  for dimension DIMENSION"
  (setf (gethash dimension *table-of-step-sizes*)
        (make-reference :val value))
  )

;;; Modified: Thu Aug 20 16:29:24 EDT 1992
;;; If no step-size defined, return 0. 
;;; Function: GET-STEP-SIZE                                  Author: raman
;;; Created: Fri Aug  7 17:52:43 1992

(defun get-step-size (dimension) 
  "retrieve step-size for this dimension"
  (or 
   (gethash dimension *table-of-step-sizes*)
   0)
  )


;;; Function: SET-STEP-SIZE                                  Author: raman
;;; Created: Fri Aug 21 09:07:24 1992

(defun set-step-size (dimension step-size) 
  "Set global step size for dimension."
  (assert (gethash dimension *table-of-step-sizes*) nil
          "Error: First define a step size  for dimension ~a "
          dimension)
  (setf (reference-val (gethash dimension *table-of-step-sizes*))
        step-size)
  )



;;; }

;;; { Maximum and minimum values

;;; { *table-of-minimum-values*
  ;;; Variable: *TABLE-OF-MINIMUM-VALUES*                      Author: raman
  ;;; Created: Mon Dec 14 10:38:37 1992

(defvar *table-of-minimum-values*
  (make-hash-table :test #'equal)
  "Table of minimum values for afl dimensions")


  ;;; Function: DEFINE-MINIMUM-VALUE                           Author: raman
  ;;; Created: Mon Dec 14 10:39:23 1992

(defun define-minimum-value (dimension min-value) 
  "Define minimum value "
  (setf (gethash dimension *table-of-minimum-values*) min-value)
  )


  ;;; Function: MINIMUM-VALUE                                  Author: raman
  ;;; Created: Mon Dec 14 10:40:10 1992

(defun minimum-value (dimension) 
  "Retrieve minimum value for this dimension"
  (gethash dimension *table-of-minimum-values*) 
  )

;;; }
;;; {*table-of-maximum-values*

  ;;; Variable: *TABLE-OF-MAXIMUM-VALUES*                      Author: raman
  ;;; Created: Mon Dec 14 10:38:37 1992

(defvar *table-of-maximum-values*
  (make-hash-table :test #'equal)
  "Table of maximum  values for afl dimensions")


  ;;; Function: DEFINE-MAXIMUM-VALUE                           Author: raman
  ;;; Created: Mon Dec 14 10:39:23 1992

(defun define-maximum-value (dimension max-value) 
  "Define maximum  value "
  (setf (gethash dimension *table-of-maximum-values*) max-value)
  )


  ;;; Function: MAXIMUM-VALUE                                  Author: raman
  ;;; Created: Mon Dec 14 10:40:10 1992

(defun maximum-value (dimension) 
  "Retrieve maximum  value for this dimension"
  (gethash dimension *table-of-maximum-values*) 
  )

;;; }

;;; { range of a dimension

  ;;; Function: DIMENSION-RANGE                                Author: raman
  ;;; Created: Mon Dec 14 11:16:39 1992

(defun dimension-range (dimension-name) 
  "Return difference between maximum and minimum values for this
dimension"
  (assert   (find dimension-name  (list-of-speech-dimensions))  nil
            "~a is not a known afl dimension"
            dimension-name)
  (- (maximum-value dimension-name)
     (minimum-value dimension-name))
  )


  ;;; Function: COMPUTE-RANGE                                  Author: raman
  ;;; Created: Mon Dec 14 17:13:51 1992

(defun compute-range (state dimension) 
  "Compute distance from maximum value for this dimension"
  (- (maximum-value dimension)
     (current-value  dimension state))
  )



  ;;; Function: LENGTH-OF-SUBINTERVAL                          Author: raman
  ;;; Created: Fri Dec 18 09:50:55 1992

(defun length-of-subinterval (dimension-name number-of-end-points) 
  "Compute length of a subinterval when dimension subdivided into
number-of-end-points -1 subintervals"
  (let
      ((interval-length (dimension-range  dimension-name ))
       (quotient (- number-of-end-points 1 )))
    (cond
      ((= 0 quotient) 0)
      (t (/ interval-length quotient ))
      ))
  )

;;; }

;;; }

;;; { *standard-voices*

;;; standard voices stored in a table.
;;; this is a hash table which for each name holds the point in speech
;;; space for that voice.

;;; Variable: *STANDARD-VOICES*                              Author: raman
;;; Created: Sat Aug  8 09:24:43 1992

(defvar *standard-voices*
  (make-hash-table :test #'equal  )
  "table of standard voices.")

;;; Modified: Tue Aug 11 10:09:32 EDT 1992
;;; Modified: Thu Aug 20 09:35:45 EDT 1992
;;; export voice being defined
;;; Function: SAVE-POINT-IN-SPEECH-SPACE                     Author: raman
;;; Created: Sun Aug  9 15:52:02 1992

(defun save-point-in-speech-space (name point) 
  "Define point  as a distinguished point in speech space called name."
  (export (list name))
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (setf  (gethash name *standard-voices*)
         point)
  )
;;; Modified: Thu Aug 20 09:34:49 EDT 1992
;;; export name of voice that is being defined.

;;; Function: DEFINE-STANDARD-VOICE                          Author: raman
;;; Created: Sat Aug  8 09:36:28 1992

(defun define-standard-voice (name &rest  settings)
  "Define a standard voice named name using settings which is a list
of dimension value pairs."
  (export (list name))
  (let ((point (make-point-in-speech-space
                :voice (create-dimension 'voice :value name)
                )))
    (dolist
        (setting settings)
      (let
          ((dimension (first setting))
           (assign  (second setting )))
        (assert (find dimension *list-of-speech-dimensions*)  nil
                "Error: Dimension ~a has not been defined, you cannot
assign a value to it"
                (first setting))
        (update-point-in-speech-space point dimension
                                      (apply #'create-dimension dimension
                                             assign ))
        )
      )
    (setf (gethash name *standard-voices*)
          point)
    )
  )
;;; Modified: Tue Aug 25 11:42:19 EDT 1992
;;; embed point in speech space before returning it. This means that
;;; those parameters that are not defined by the voice will assume
;;; global values that are currently set.  Earlier I was calling
;;; embed-point-in-speech-space within the define-standard-voice
;;; function and this mean that the voices got defined with the
;;; default values that were defined at the time the language is
;;; compiled.

;;; Function: GET-POINT-IN-SPEECH-SPACE Author: raman
;;; Created: Sat Aug  8 09:30:57 1992
;;; Modified: Thu Aug 20 11:28:52 EDT 1992
;;; returns multiple values, point and a list of dimensions

(defun get-point-in-speech-space (name) 
  "return predefined point associated with name"
  (values 
   (embed-point-in-speech-space (gethash name *standard-voices*))
   *list-of-speech-dimensions*)
  )

;;; }

