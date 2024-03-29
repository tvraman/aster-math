;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

;;; Mon Dec 28 14:02:10 EST 1992 Cross references.  When a label is
;;; seen, install it in the label table along with a pointer to the
;;; object being labelled.  When a reference is seen, retrieve the
;;; corresponding label and the object pointed to from the table and
;;; call the method read-cross-reference on that object.  This method
;;; can decide how to handle the cross-reference, either read the
;;; title, read an informative label, offer the option to jump there
;;; etc.  Current recognizer picks up labels for sectional units.
;;; Need to handle labels for other objects that take labels in Latex
;;; eg, equations, etc.
;;;

(defparameter *follow-cross-ref-wait* 0
  "Follow cross reference behaviour. ")

(defun follow-cross-reference?()
  "Prompt user for following cross reference. If yes, caller reads
  cross referenced object and continue.
 Behaviour determined by *follow-cross-ref-wait*. If 0, do not prompt.
If non-zero, prompt, waiting for *follow-cross-ref-wait* seconds. "
  (unless (zerop *follow-cross-ref-wait*)
    (y-or-n-p "Follow cross ref? ")))

  ;;; Method: READ-ALOUD                                       Author: raman
  ;;; Created: Mon Dec 28 15:17:11 1992

(defmethod read-aloud  ((cross-ref cross-ref))
  "Read aloud a cross reference"
  (let  ((cross-reference-label  (find-cross-reference cross-ref )))
    (when cross-reference-label
      (read-cross-reference (points-to cross-reference-label  )))
    (when (follow-cross-reference?)
      (save-pointer-excursion
       (follow-cross-reference cross-ref )))))

  ;;; Method: FOLLOW-CROSS-REFERENCE                           Author: raman
  ;;; Created: Fri Apr 30 13:27:57 1993

(defmethod follow-cross-reference ((cross-ref cross-ref))
  "Follow cross reference"
  (let* ((cross-reference-label  (find-cross-reference cross-ref ))
         (*get-label-wait* 0)
         (ref-object (when cross-reference-label
                       (points-to cross-reference-label  ))))
    (when cross-reference-label
      (afl:new-block
       (afl:local-set-state (afl-state ref-object ))
       (read-aloud ref-object )))))

  ;;; Method: READ-CROSS-REFERENCE                             Author: raman
  ;;; Created: Mon Dec 28 15:18:32 1992

(defmethod read-cross-reference ((sectional-unit sectional-unit))
  "Read a sectional unit cross reference"
  (afl:tts-queue " ")
  (read-aloud (sectional-unit-name sectional-unit ))
  (when (sectional-unit-number sectional-unit)
    (afl:tts-queue  (format nil "~a" (sectional-unit-number sectional-unit ))))
  (afl:comma-intonation)
  (read-aloud (title  sectional-unit ))
  (afl:comma-intonation)
  (afl:tts-force))

  ;;; Method: READ-CROSS-REFERENCE                             Author: raman
  ;;; Created: Mon Dec 28 16:59:39 1992

(defmethod read-cross-reference ((math-equation math-equation))
  "Read a cross reference equation"
  (cond
    ((label math-equation)
     (read-aloud (label-name (label  math-equation ))))
    (t (afl:tts-queue  "equation")
       (read-aloud math-equation ))))

(defmethod read-cross-reference ((new-environment new-environment))
  "Read cross referenced environment "
  (cond
    ((label new-environment)
     (read-aloud (label-name (label new-environment ))))
    (t (read-aloud (name new-environment )))))

(defmethod read-cross-reference ((figure figure ))
  "Read cross referenced figure"
  (cond
    ((label figure)
     (read-aloud (label-name (label figure )))
     (afl:tts-force)
     (afl:tts-silence 1)
     (read-aloud (caption figure)))
    (t (read-aloud "figure captioned, ")
       (read-aloud (caption  figure )))))

(defmethod read-cross-reference ((table table ))
  "Read cross referenced table"
  (cond
    ((label table)
     (read-aloud (label-name (label table )))
     (afl:tts-force)
     (afl:tts-silence 1)
     (read-aloud (caption table)))
    (t
     (read-aloud "table captioned, ")
     (read-aloud (caption  table )))))

(defmethod read-cross-reference ((labelled-class labelled-class ))
  "read cross references that are labelled"
  (when (label labelled-class)
    (read-aloud (label-name (label labelled-class  )))))

(defmethod read-cross-reference((object t )) nil)

(defmethod points-to ((ordinary t )) nil)
