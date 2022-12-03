;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :afl)

(export
 '(step-size move-to move-by scale-by step-by
   multi-move-by multi-move-to multi-scale-by multi-step-by
   generalized-afl-operator))

;;; Each fold contains  an operator
;;{{{ move-by
;;; Function: MOVE-BY                                        Author: raman
;;; Created: Sun Aug  9 15:55:50 1992

(defun   move-by (point   dimension offset
                  &key(slot  'value))
  "Return point reached by moving from point along dimension by offset.
Default is to vary the value assigned to dimension.
If called with :slot 'step-size, modifies the step size instead."
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space. " point)
  (assert (find dimension (speech-dimensions)) nil
          "move-by: Invalid dimension ~a" dimension)
  (let*
      ((new-point (copy-point-in-speech-space point))
       (new-dimension (copy-dimension (point-accessor dimension new-point ))))
    (setf (dimension-accessor slot new-dimension)
          (+
           (reference-value (  dimension-accessor slot new-dimension))
           offset))
    (values
     (update-point-in-speech-space new-point dimension new-dimension)
     (if (eq 'value slot)
         (list dimension)               
         nil))))

;;}}}
;;{{{ step-by

;;; Function: STEP-BY                                           Author: raman
;;; Created: Sun Aug  9 17:48:50 1992

(defun   step-by (point   dimension n-steps
                  &key (slot 'value))
  "Return point reached by moving from point by n-steps along
dimension dimension. Default is to vary value assigned along
dimension.
If called with :slot 'step-size, modifies the step size instead."
  (assert (find dimension (speech-dimensions)) nil
          "step-by: Invalid dimension ~a" dimension)
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space. " point)
  (let* ((new-point (copy-point-in-speech-space point))
       (new-dim (copy-dimension (point-accessor dimension new-point  ))))
    (setf (dimension-accessor slot  new-dim)
          (+
           (reference-value (dimension-accessor slot  new-dim))
           (* n-steps
              (reference-value (dimension-step-size new-dim )))))
    (values
     (update-point-in-speech-space new-point dimension new-dim)
     (if (eq 'value slot)
         (list dimension)
         nil))))

;;}}}
;;{{{move-to

;;; Function: MOVE-TO                                        Author: raman
;;; Created: Sun Aug  9 17:53:36 1992

(defun  move-to (point dimension value &key(slot 'value))
  "Return point reached by setting value along dimension dimension to
value. Default is to change the value assigned along this dimension.
If called with :slot 'step-size, modifies the step size instead. "
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space" point)
  (assert (find dimension (speech-dimensions)) nil
          "move-to: Invalid dimension ~a" dimension)
  (let* ((new-point (copy-point-in-speech-space point))
         (new-dim(copy-dimension (point-accessor dimension new-point ))))
    (setf (dimension-accessor slot  new-dim)
          (reference-value (dimension-accessor slot  new-dim )))
    (values
     (update-point-in-speech-space new-point dimension new-dim)
     (if (eq 'value slot)
         (list dimension)
         nil))))

;;}}}
;;{{{scale-by

(defun   scale-by (point dimension scale-factor &key (slot 'value))
  "Return point reached by scaling value along  dimension by scale factor
If called with :slot 'step-size, modifies the step size instead."
  (assert (point-in-speech-space-p point) nil
          "~a is not a point in speech space" point)
  (assert (find dimension (speech-dimensions)) nil
          "scale-by: Invalid dimension ~a" dimension)
  (let* ((new-point (copy-point-in-speech-space point))
       (new-dim(copy-dimension (point-accessor dimension new-point ))))
    (setf (dimension-accessor slot  new-dim)
          (* scale-factor
             (reference-value (dimension-accessor slot  new-dim ))))
    (values
     (update-point-in-speech-space new-point dimension new-dim)
     (if (eq 'value slot)
         (list dimension)
         nil))))

;;}}}
;;{{{ multi-move-by

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
