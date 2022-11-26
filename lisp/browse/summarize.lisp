;;;   -*-   Mode: LISP -*-    ;;;



;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Tue Jan 12 08:57:40 EST 1993
;;; Code to summarize an object


  ;;; Method: SUMMARIZE                                        Author: raman
  ;;; Created: Tue Jan 12 08:57:51 1993

(defun read-succinctly (object)
  "Read object succinctly. "
  (if (substitution object)
      (read-aloud (substitution object ))
      (read-aloud (type-of object  )))
  )

  ;;; Method: SUMMARIZE                                        Author: raman
  ;;; Created: Mon Sep 27 20:32:35 1993

(defmethod summarize ((math-array math-array))
  "Summarize a math array."
  (read-aloud "array. ")
  (afl:tts-force))
;; (defmethod summarize((list list ))
;;   "Summarize a list of things"
;;   (let ((first-type (type-of (first list ))))
;;     (cond
;;       ((every #'(lambda(item)
;;                   (typep item first-type)) list)
;;        (read-aloud first-type)
;;        (afl:tts-queue  " list. ")
;;        )
;;       (t  (afl:tts-queue (format nil "~a list. "
;;                                  (class-name
;;                                   (closest-common-superclass  list  ))))
;;           )
;;       )
;;     )
;;   )

(defun say-what-this-is-called(thing)
  "Say this thing is "
  (let ((name (this-argument-is-called thing )))
    (when name
      (read-aloud name )
      (afl:tts-queue " is, ")
      (afl:tts-force )))
  )

(defun say-what-this-attribute-is-called(thing)
  "Say what this attribute is called."
  (let ((name (this-attribute-is-called thing )))
    (when name
      (read-aloud name )
      (afl:tts-queue " is, ")
      (afl:tts-force )))
  )

(defmethod summarize ((math-object math-object ))
  "Summarize math object. "
  (save-pointer-excursion
    (say-what-this-is-called math-object)
    (afl:tts-queue " ")
    (if(and  (leaf-p math-object )
             (= 1 (weight math-object)))
       (read-current)
       (read-succinctly  math-object))
    (afl:tts-force )))

(defmethod summarize ((paragraph paragraph))
  "Summarize a paragraph"
  (afl:tts-speak "Paragraph"))

(defmethod summarize  ((slide slide ))
  (save-pointer-excursion
    (read-aloud
     (first (contents slide))))
  )
(defmethod summarize ((math-subformula math-subformula ))
  "Summarize math subformula"
  (save-pointer-excursion
    (say-what-this-is-called math-subformula)
    (afl:tts-queue " ")
    (cond
      ((leaf-p math-subformula )
       (read-current))
      ((null (attributes math-subformula ))
       (summarize (contents  *read-pointer* )))
      (t (let ((attributes (attributes math-subformula )))
           (read-succinctly math-subformula )
           (loop for attr in attributes do
             (read-aloud attr)
             (afl:comma-intonation )))
         ))
    (afl:tts-force )))

(defmethod summarize ((arrow-operator arrow-operator ))
  "Summarize arrow operator "
  (save-pointer-excursion
    (say-what-this-is-called arrow-operator)
    (afl:with-pronunciation-mode (:mode :math)
      (read-aloud (contents arrow-operator )))
    (afl:tts-force)
    )
  )

(defmethod summarize ((relational-operator relational-operator ))
  "Summarize relational operator "
  (save-pointer-excursion
    (say-what-this-is-called relational-operator)
    (afl:with-pronunciation-mode (:mode :math)
      (read-aloud (contents relational-operator )))
    (afl:tts-force)
    )
  )

(defmethod summarize ((binary-operator binary-operator ))
  "Summarize binary operator "
  (save-pointer-excursion
    (say-what-this-is-called binary-operator)
    (afl:with-pronunciation-mode (:mode :math)
      (read-aloud (contents binary-operator)))
    (afl:tts-force)
    )
  )

(defmethod summarize ((juxtaposition juxtaposition))
  "Summarize juxtaposition object"
  (save-pointer-excursion
    (say-what-this-is-called juxtaposition )
    (if (special-pattern juxtaposition )
        (read-aloud (special-pattern juxtaposition ))
        (afl:tts-queue "juxtaposition. "))
    (when (substitution juxtaposition)
      (read-aloud (substitution juxtaposition )))
    (afl:tts-force))
  )

(defmethod summarize ((fraction fraction))
  (say-what-this-is-called fraction )
  (read-succinctly fraction)
  (afl:tts-force ))

(defmethod summarize ((mathematical-function-name mathematical-function-name ))
  "Summarize functions. "
  (save-pointer-excursion
    (say-what-this-is-called mathematical-function-name )
    (afl:tts-queue " ")
    (if   (leaf-p (children mathematical-function-name ))
          (read-current)
          (read-succinctly     mathematical-function-name  ))
    (afl:tts-force)
    )
  )

(defmethod summarize ((ordinary t))
  "Summarize by default gives type of object"
  (read-aloud (type-of ordinary ))
  (afl:tts-force )
  )

  ;;; Method: SUMMARIZE                                        Author: raman
  ;;; Created: Tue Jan 12 08:58:49 1993

(defmethod summarize ((sectional-unit sectional-unit))
  "Summarize sectional unit"
  (let ((save-state *read-pointer* ))
    (read-aloud (sectional-unit-name sectional-unit ))
    (afl:tts-queue " ")
    (afl:tts-queue (format nil "~a"  (sectional-unit-number sectional-unit )))
    (read-aloud (title sectional-unit ))
    (afl:tts-force)
    (type-of (setf *read-pointer* save-state )))
  )

(defmethod summarize ((article article))
  "Summarize an article"
  (cond
    ((title article)
     (save-pointer-excursion
       (read-aloud (title article ))))
    (t (afl:tts-queue "article. "))))

(defmethod summarize ((document document))
  "Summarize a document"
  (afl:tts-speak (format nil "~a" (type-of document)))
  (afl:tts-force))

(defmethod summarize ((attribute attribute ))
  "Summarize an attribute. "
  (save-pointer-excursion
    (say-what-this-attribute-is-called attribute )
    (if (leaf-p (attribute-value attribute ))
        (read-current)
        (read-succinctly  (attribute-value attribute )))
    (afl:tts-force)
    )
  )
