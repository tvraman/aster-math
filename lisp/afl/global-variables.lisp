;;;   -*-   Mode: LISP -*-    ;;;
 
(in-package :afl)

(export '(get-point-in-speech-space standard-voices))
;;; Each fold  contains a variable and its associated accessors and
;;; modifiers.
;;; Contains all defvars etc used by afl.

;;{{{ *global-values*

;;; Variable: *GLOBAL-VALUES*                              Author: raman
;;; Created: Fri Aug  7 12:46:20 1992
(defvar *global-values* (make-hash-table :test #'equal  )
  "Default parameter settings")

(defun global-values ()
  "Return default settings."
  *global-values*)

;;;  Function: DEFINE-DEFAULT-VALUE Author: raman
;;; Created: Fri Aug  7 12:49:30 1992
(defun  define-default-value (dimension value)
  "Assign VALUE as the default value along dimension DIMENSION."
  (setf (gethash dimension (global-values))
        (make-reference :val  value)))

;;; Function: GET-GLOBAL-VALUE                                  Author: raman
;;; Created: Fri Aug  7 13:29:55 1992
(defun  get-global-value  (dimension) 
  "return default value for dimension dimension."
  (gethash dimension (global-values)))

;;; Function: SET-GLOBAL-VALUE                               Author: raman
;;; Created: Wed Aug 26 15:18:24 1992

(defun set-global-value (dimension value) 
  "Update default value; to set for the first time, use define-default-value"
  (assert (gethash dimension (global-values)) nil
          "Error: First define a default value for dimension ~a " dimension)
  (setf (reference-val (gethash dimension (global-values))) value))

;;}}}
;;{{{ *table-of-units*
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

;;}}}
;;{{{*table-of-synthesizer-codes*

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
  (gethash dimension *table-of-synthesizer-codes*))

;;}}}
;;{{{ *table-of-step-sizes*

;;; Variable: *TABLE-OF-STEP-SIZES* Author: raman
;;; Created: Fri Aug  7 17:49:29 1992

(defvar *table-of-step-sizes*
  (make-hash-table :test #'equal  )
  "table containing step sizes   for each dimension")
;;; Function: DEFINE-STEP-SIZE                               Author: raman
;;; Created: Fri Aug  7 17:51:13 1992

(defun define-step-size (dimension value) 
  "Define VALUE as the step-size  for dimension DIMENSION"
  (setf (gethash dimension *table-of-step-sizes*)
        (make-reference :val value))
  )
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
  "Set default step size for dimension."
  (assert (gethash dimension *table-of-step-sizes*) nil
          "Error: First define a step size  for dimension ~a "
          dimension)
  (setf (reference-val (gethash dimension *table-of-step-sizes*))
        step-size)
  )



;;}}}
;;{{{ Maximum and minimum values

;;{{{ *table-of-minimum-values*
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

;;}}}
;;{{{*table-of-maximum-values*

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

;;}}}

;;{{{ range of a dimension

  ;;; Function: DIMENSION-RANGE                                Author: raman
  ;;; Created: Mon Dec 14 11:16:39 1992

(defun dimension-range (dimension-name) 
  "Return difference between maximum and minimum values for this
dimension"
  (assert   (find dimension-name  (speech-dimensions))  nil
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

;;}}}

;;}}}
;;{{{ *standard-voices*

;;; standard voices stored in a table.
;;; this is a hash table which for each name holds the point in speech
;;; space for that voice.

;;; Variable: *STANDARD-VOICES*                              Author: raman
;;; Created: Sat Aug  8 09:24:43 1992

(defvar *standard-voices*
  (make-hash-table :test #'equal  )
  "table of standard voices.")
(defun standard-voices ()
  "Return table of standard voices."
  *standard-voices*)

;;; Function: DEFINE-STANDARD-VOICE                          Author: raman
;;; Created: Sat Aug  8 09:36:28 1992

(defun define-standard-voice (name &rest  settings)
  "Define a standard voice named name using settings which is a list
of dimension value pairs."
  (export name)
  (let ((point (make-point-in-speech-space
                :voice (create-dimension 'voice :value name))))
    (dolist
        (setting settings)
      (let
          ((dimension (first setting))
           (assign  (second setting )))
        (assert (find dimension (speech-dimensions))  nil
                "Error: Dimension ~a has not been defined, you cannot
assign a value to it"
                (first setting))
        (update-point-in-speech-space point dimension
                                      (apply #'create-dimension dimension
                                             assign ))
        )
      )
    (setf (gethash name (standard-voices)) point)))

;;; Function: GET-POINT-IN-SPEECH-SPACE Author: raman
;;; Created: Sat Aug  8 09:30:57 1992
(defun get-point-in-speech-space (name) 
  "return predefined point associated with name"
  (values (embed-point-in-speech-space (gethash name (standard-voices)))
   (speech-dimensions)))

;;}}}
