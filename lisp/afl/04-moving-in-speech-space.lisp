;;;   -*-   Mode: LISP -*-    ;;;
 
 
;;{{{ Introduction

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)


(export '(
          value step-size ;;;slots
          move-to move-by scale-by step-by
          multi-move-by multi-move-to multi-scale-by multi-step-by
          generalized-afl-operator))
;;; Modified: Wed Mar 24 17:17:27 EST 1993
;;; turning all the move operators into methods makes them too slow
;;; resulting in undesirable pauses. For the present leaving move-to
;;; as  a method since the sound audio component also defines a
;;; method on this generic function.  For the present making all other
;;; operators functions for efficiency. If other components need to
;;; introduce their own methods on these operators, then uncomment the
;;; defgeneric form, and make the necessary change to the lambda list
;;; of the defmethod forms.
;;; Modified: Wed Mar 24 08:56:20 EST 1993
;;; turning into methods so that other component spaces can
;;; implement their own methods on these generic operators.
;;; Implements the move operators as discussed in the notes on audio
;;; formatting primitives.
;;; Refer to: <(notes on audio formatting)>
;;; The operators return points in speech space. There are four move operators.

;;}}}
;;; Each fold contains  an operator
;;{{{ move-by

;;; Modified: Wed Mar 24 08:57:17 EST 1993
;;; turned into method.
;;; Modified: Wed Aug 19 08:50:02 EDT 1992
;;; Adding keyword argument slot with default value
;;; 'value   so that by default move operators change the
;;; value slot. Having this keyword argument allows the program to
;;; cleanly modify step size if necessary by specifying  :slot
;;; 'step-size
;;; Modified: Thu Aug 20 10:51:37 EDT 1992
;;; return both new state and dimension along which change made.
;;; Modified: Sun Aug 23 08:32:47 EDT 1992
;;; validate dimensions
;;; Modified: Sat Oct 17 12:13:19 EDT 1992
;;; If slot step-size modified, changed-dimensions is nil.
;;; Function: MOVE-BY                                        Author: raman
;;; Created: Sun Aug  9 15:55:50 1992

(defun   move-by (point   dimension offset
                  &key(slot  'value))
  "Return point reached by moving from point along dimension by offset.
Default is to vary the value assigned to dimension.
If called with :slot 'step-size, modifies the step size instead."
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space. "
          point)
  (assert (find dimension (speech-dimensions)) nil
          "move-by: Invalid dimension ~a"
          dimension)
  (let*
      ((new-point (copy-point-in-speech-space point))
       (new-dimension (copy-dimension
                       (point-accessor dimension new-point ))))
    (setf (dimension-accessor slot new-dimension)
          (+
           (reference-value (  dimension-accessor slot new-dimension))
           offset))
    (values
     (update-point-in-speech-space new-point dimension new-dimension)
     (if (eq 'value slot)
         (list dimension)               ; value modified
         nil)                             ; slot modified
     )))

;;}}}
;;{{{ step-by

;;; Modified: Wed Aug 19 09:23:37 EDT 1992

;;; added keyword argument :slot with default 'value
;;; Doing this to maintain consistency with the other move operators,
;;; it does not make too much sense to call step-by with :slot 'step-size
;;; Modified: Thu Aug 20 11:12:29 EDT 1992
;;; returning multiple values point and dimension that is changed
;;; Modified: Sun Aug 23 08:32:47 EDT 1992
;;; validate dimensions
;;; Modified: Sat Oct 17 12:13:19 EDT 1992
;;; If slot step-size modified, changed-dimensions is nil.
;;; Function: STEP-BY                                           Author: raman
;;; Created: Sun Aug  9 17:48:50 1992

(defun   step-by (point   dimension number-of-steps
                  &key (slot 'value))
  "Return point reached by moving from point by number-of-steps along
dimension dimension. Default is to vary value assigned along
dimension.
If called with :slot 'step-size, modifies the step size instead."
                                        ;  (format t "~&~a= dimension
                                        ;~&~a=current value
                                        ;~&~a=number-of-steps
                                        ;~&~a = step-size "
                                        ;        dimension
                                        ;        (current-value dimension)
                                        ;        number-of-steps
                                        ;        (current-step-size dimension ))
  (assert (find dimension (speech-dimensions)) nil
          "step-by: Invalid dimension ~a"
          dimension)
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space. "
          point)
  (let*
      ((new-point (copy-point-in-speech-space point))
       (new-dimension (copy-dimension
                       (point-accessor dimension new-point  )))
       )
    (setf (dimension-accessor slot  new-dimension)
          (+
           (reference-value (dimension-accessor slot  new-dimension))
           (* number-of-steps
              (reference-value (dimension-step-size new-dimension )))))
    (values
     (update-point-in-speech-space new-point dimension new-dimension)
     (if (eq 'value slot)
         (list dimension)               ; value modified
         nil)                             ; slot modified
     )
    )
  )

;;}}}
;;{{{move-to

  ;;; Generic: MOVE-TO                                         Author: raman
  ;;; Created: Wed Mar 24 09:45:44 1993

(defgeneric move-to (point dimension value &key &allow-other-keys)
  (:documentation "Move to value along dimension from point and return result")
  )

;;; Modified: Wed Aug 19 09:27:20 EDT 1992
;;; Added keyword argument :slot with default 'value
;;; Modified: Thu Aug 20 11:13:21 EDT 1992
;;; Return multiple values: new point nad name of changed dimension
;;; Modified: Sun Aug 23 08:32:47 EDT 1992
;;; validate dimensions
;;; Modified: Fri Aug 28 12:00:37 EDT 1992
;;; validate point
;;; Modified: Sat Oct 17 12:13:19 EDT 1992
;;; If slot step-size modified, changed-dimensions is nil.
;;; Function: MOVE-TO                                        Author: raman
;;; Created: Sun Aug  9 17:53:36 1992

(defmethod  move-to (point dimension value
                     &key(slot 'value))
  "Return point reached by setting value along dimension dimension to
value. Default is to change the value assigned along this dimension.
If called with :slot 'step-size, modifies the step size instead. "
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (assert (find dimension (speech-dimensions)) nil
          "move-to: Invalid dimension ~a"
          dimension)
  (let*
      ((new-point (copy-point-in-speech-space point))
       (new-dimension(copy-dimension
                      (point-accessor dimension new-point )))
       )
    (setf (dimension-accessor slot  new-dimension)
          value)
    (values
     (update-point-in-speech-space new-point dimension new-dimension)
     (if (eq 'value slot)
         (list dimension)               ; value modified
         nil)                             ; slot modified
     )
    )
  )

;;}}}
;;{{{scale-by

;;; Modified: Sat Oct 17 12:10:45 EDT 1992
;;; If :slot specified as step-size then changed-dimensions returned
;;; as nil.
;;; Function: SCALE-BY                                          Author: raman
;;; Created: Sun Aug  9 17:56:53 1992

(defun   scale-by (point dimension scale-factor
                   &key (slot 'value))
  "Return point reached by scaling value along  dimension by scale factor
If called with :slot 'step-size, modifies the step size instead."
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (assert (find dimension (speech-dimensions)) nil
          "scale-by: Invalid dimension ~a"
          dimension)
  (let*
      ((new-point (copy-point-in-speech-space point))
       (new-dimension(copy-dimension
                      (point-accessor dimension new-point )))
       )
    (setf (dimension-accessor slot  new-dimension)
          (* scale-factor
             (reference-value (dimension-accessor slot  new-dimension ))))
    (values
     (update-point-in-speech-space new-point dimension new-dimension)
     (if (eq 'value slot)
         (list dimension)               ; value modified
         nil))                            ; step-size modified
    )
  )

;;}}}
;;{{{ multi-move-by

;;; Modified: Thu Aug 20 11:14:50 EDT 1992
;;; return multiple values new point and name of changed dimension
;;; Modified: Sat Oct 17 12:45:40 EDT 1992
;;; Fixed the returning of changed dimensions, collects and return
;;; what the simple move operators return. This is so that if  simple
;;; move operator returns nil for changed dimensions, then this value
;;; is passed up.
;;; Function: MULTI-MOVE-BY                                  Author: raman
;;; Created: Wed Aug 12 09:49:55 1992

(defun   multi-move-by (point  &rest settings)
  "Move from point along several dimensions, specify settings as
dimension value pairs"
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (let
      ((new-point (copy-point-in-speech-space point ))
       (dimension nil)
       (changed-dimensions nil))
    (dolist (setting settings)
      (multiple-value-setq ( new-point dimension)
        (apply #'move-by
               new-point
               setting)))
    (setf changed-dimensions (append
                              changed-dimensions
                              dimension))
    (values new-point
            changed-dimensions
            )
    )
  )

;;}}}
;;{{{ multi-move-to

;;; Modified: Thu Aug 20 11:39:36 EDT 1992
;;; return multiple values: new point and list of changed dimensions
;;; Modified: Sat Oct 17 12:45:40 EDT 1992
;;; Fixed the returning of changed dimensions, collects and return
;;; what the simple move operators return. This is so that if  simple
;;; move operator returns nil for changed dimensions, then this value
;;; is passed up.
;;; Function: MULTI-MOVE-TO                                  Author: raman
;;; Created: Wed Aug 12 09:56:09 1992

(defun   multi-move-to (point   &rest settings)
  "Move along multiple dimensions, settings specified as dimension value pairs"
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (let
      ((new-point (copy-point-in-speech-space point ))
       (dimension nil)
       (changed-dimensions nil))
    (dolist (setting settings)
      (multiple-value-setq ( new-point dimension)
        (apply #'move-to
               new-point
               setting))
      (setf changed-dimensions (append
                                changed-dimensions
                                dimension))
      )
    (values new-point
            changed-dimensions)
    )
  )

;;}}}
;;{{{ multi-scale-by

;;; Modified: Thu Aug 20 11:40:47 EDT 1992
;;; return multiple values new point and list of changed dimensions
;;; Modified: Sat Oct 17 12:45:40 EDT 1992
;;; Fixed the returning of changed dimensions, collects and return
;;; what the simple move operators return. This is so that if  simple
;;; move operator returns nil for changed dimensions, then this value
;;; is passed up.
;;; Function: MULTI-SCALE-BY                                 Author: raman
;;; Created: Wed Aug 12 09:57:31 1992

(defun   multi-scale-by (point  &rest settings)
  "scale along multiple dimensions, specify settings as dimension value pairs"
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (let
      ((new-point (copy-point-in-speech-space point ))
       (dimension nil)
       (changed-dimensions nil))
    (dolist (setting settings)
      (multiple-value-setq ( new-point dimension)
        (apply #'scale-by
               new-point
               setting))
      (setf changed-dimensions (append
                                changed-dimensions
                                dimension))
      )
    (values new-point
            changed-dimensions)
    )
  )

;;}}}
;;{{{ multi-step-by

;;; Modified: Thu Aug 20 11:41:10 EDT 1992
;;; return multiple values new point and list of changed dimensions
;;; Modified: Sat Oct 17 12:45:40 EDT 1992
;;; Fixed the returning of changed dimensions, collects and return
;;; what the simple move operators return. This is so that if  simple
;;; move operator returns nil for changed dimensions, then this value
;;; is passed up.
;;; Function: MULTI-STEP-BY                                  Author: raman
;;; Created: Wed Aug 12 09:58:30 1992

(defun   multi-step-by (point &rest settings)
  "step along multiple dimensions"
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (let
      ((new-point (copy-point-in-speech-space point ))
       (dimension nil)
       (changed-dimensions nil))
    (dolist (setting settings)
      (multiple-value-setq ( new-point dimension)
        (apply #'step-by
               new-point
               setting))
      (setf changed-dimensions (append
                                changed-dimensions
                                dimension))
      )
    (values new-point
            changed-dimensions)
    )
  )

;;}}}
;;{{{ Generalized operator.

;;; Variable: *VALID-MOVES*                                  Author: raman
;;; Created: Mon Sep  7 19:32:13 1992

(defvar *valid-moves*
  (list 'move-by 'move-to 'scale-by 'step-by)
  "valid moves for generalized operator")

  ;;; Generic: GENERALIZED-AFL-OPERATOR                        Author: raman
  ;;; Created: Wed Mar 24 10:09:20 1993
;;; Function: GENERALIZED-AFL-OPERATOR Author: raman
;;; Created: Mon Sep  7 19:35:04 1992

(defun  generalized-afl-operator (point   &rest settings)
  "Operate on point  and return new point. "
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space"
          point)
  (let
      ((new-point (copy-point-in-speech-space point )))
    (dolist (setting settings)
      (let
          ((operator (first setting )))
        (assert  (member  operator *valid-moves*) nil
                 "~a is not a valid operator" operator)
        (setf new-point
              (apply operator
                     new-point
                     (rest setting )))
        )
      )
    (values new-point
            (mapcar #'(lambda(x) (second x)) settings))
    )
  )

;;}}}
