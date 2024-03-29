;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)
;;; This file contains reading rule definitions using the new
;;; defmethod reading-rule.
;;; Will eventually replace the reading rules in math-reading-rules.lisp
 
(def-reading-rule (math-object simple)
  "Simple reading rule for math objects "
  (let*
      ((pause-duration  (compute-pause math-object )))
    (afl:with-surrounding-pause pause-duration
      (cond
        ((leaf-p math-object) (read-math-object-and-attributes math-object))
        ( (read-as-infix?  math-object) (read-as-infix math-object))
        ((read-as-prefix?  math-object) (read-as-prefix math-object))
        ((children math-object) (read-aloud (children math-object)))
        (t (error "Do not know what to do ~%"))
        )))
  )

(def-reading-rule (diagonal-dots simple)
  "Reading rule for diagonal dots. "
  (afl:new-block
    (loop for
          i from 1 to 3 do
            (afl:tts-queue "and so on")
            (afl:low-intonation)
            (afl:tts-force))))

(def-reading-rule (vertical-dots simple)
  "Simple reading rule for vertical dots. "
  (afl:new-block
    (loop
      for i from 1 to 3 do
        (afl:tts-queue "dot")
        (afl:low-intonation)))
  (afl:tts-force ))

(def-reading-rule(phantom  simple) nil)
(def-reading-rule (v-phantom simple) nil)

(def-reading-rule (generalized-root simple)
  "a simple reading rule for radicals"
  (let*
      ((children (children generalized-root ))
       (nth-root-of (first children ))
       (radical-arg (second children  )))
    (read-aloud  (cardinal-number nth-root-of ))
    (read-aloud " root of ")
    (with-reading-state (reading-state 'children )
      (read-aloud radical-arg  )))
  )

(def-reading-rule (fraction simple)
  "simple reading rule for fractions "
  (let ((pause-amount  (compute-pause fraction ))
        (parent (parent fraction)))
    (unless (equal 'root parent )
      (afl:comma-intonation)
      (afl:tts-queue  "fraction ")
      (afl:comma-intonation))
    (afl:with-surrounding-pause pause-amount
      (cond
        ((and (leaf-p (numerator-of fraction))
              (leaf-p (denominator-of fraction )))
         (read-aloud (numerator-of fraction))
         (when (> (weight (numerator-of fraction )) 1)
           (afl:comma-intonation))
         (afl:tts-queue "over" )
         (read-aloud (denominator-of fraction ))
         (afl:comma-intonation))
        (t (with-reading-state (reading-state 'children)
             (read-aloud (numerator-of fraction )))
           (afl:comma-intonation)
           (afl:tts-queue  " divided by,  ")
           (with-reading-state (reading-state 'children)
             (read-aloud (denominator-of fraction )))))
      ))
  )

(def-reading-rule (delimited-expression simple)
  " Simple reading rule for delimited expressions "
  (afl:with-surrounding-pause (* *pause-around-child*
                                 (weight  delimited-expression))
    (read-aloud(open-delimiter delimited-expression ))
    (with-reading-state (reading-state 'children)
      (loop for child in (children delimited-expression)
            do           (read-aloud child  )))
    (when  (or  (attributes delimited-expression)
                (eql 'mismatched-delimiters
                     (delimited-expression-type delimited-expression)))
      (read-aloud (close-delimiter delimited-expression)))
    (when (attributes delimited-expression)
      (mapc #'read-aloud
            (sorted-attributes  (attributes
                                 delimited-expression )))))
  )
(def-reading-rule (parenthesis simple)
  " Simple reading rule for delimited expressions "
  (afl:with-surrounding-pause (* *pause-around-child*
                                 (weight  parenthesis))
    (read-aloud "quantity")
    (with-reading-state (reading-state 'children)
      (loop for child in (children parenthesis)
            do           (read-aloud child  )))
    (when  (attributes parenthesis)

      (read-aloud "close quantity "))
    (when (attributes parenthesis )
      (mapc #'read-aloud
            (sorted-attributes  (attributes
                                 parenthesis )))))
  )

(def-reading-rule (math-subformula simple)   "Simple reading rule for math subformula"
  (read-math-object-and-attributes math-subformula)
  )

(def-reading-rule (juxtaposition simple)
  "Simple reading rule for juxtaposition"
  (let
      ((pause-amount (compute-pause juxtaposition )))
    (afl:with-surrounding-pause pause-amount
      (read-aloud (children juxtaposition )))
    )
  )

(def-reading-rule (juxtaposition times)
  "Read juxtaposition as times"
  (let ((pause-amount (compute-pause juxtaposition ))
        (children (children juxtaposition )))
    (afl:with-surrounding-pause pause-amount
      (read-aloud (first  children))
      (loop for child in  (rest children)
            do
               (read-aloud  "times" )
               (read-math-child   child))
      ))
  )

(def-reading-rule  (math-eqnarray simple)
  "Simple reading rule for eqnarray: uses directional audio"
  (afl:with-pronunciation-mode (:mode :math)
    (loop for equation  in  (contents math-eqnarray)
          do
             (let
                 ((left-hand-side   (first equation))
                  (relational (second equation))
                  (right-hand-side (third equation )))
               (when left-hand-side
                 (with-reading-state (reading-state 'left-hand-side)
                   (read-aloud left-hand-side)
                   (afl:tts-force )))
               (read-aloud relational)
               (when right-hand-side
                 (with-reading-state (reading-state 'right-hand-side)
                   (read-aloud right-hand-side)
                   (afl:tts-force ))))))
  )

(def-reading-rule (relational-operator directional)
  "Read relational operators using directional audio"
  (let
      ((relational (contents relational-operator))
       (children (children relational-operator )))
    (cond
      ((= (length children) 2)
       (afl:with-pronunciation-mode (:mode :math)
         (afl:with-surrounding-pause  (compute-pause
                                       (first children))
           (with-reading-state (reading-state  'left-hand-side )
             (read-aloud (first children))
             ))
         (read-aloud relational)
         (afl:with-surrounding-pause (compute-pause
                                      (second children))
           (with-reading-state (reading-state 'right-hand-side)
             (read-aloud (second children ))))))
      (t (loop for equation in (equations relational-operator)
               do                (read-aloud equation) )
         )
      ))
  )

(def-reading-rule (math-equation simple)
  "Simple reading rule for equations. Uses directional audio"
  (let ((equation (contents math-equation ))
        (number (anumber math-equation ))
        (label-name
          (when (label math-equation)
            (label-name (label math-equation  )))))
    (afl:new-block
      (afl:set-pronunciation-mode :math)
      (afl:local-set-state (reading-state 'math ))
      (read-aloud equation )
      (afl:tts-silence 5)
      (if label-name
          (read-aloud label-name)
          (read-aloud  (format nil "math equation ~a" number  )))
      (afl:tts-force )
      (relabel-if-necessary (label math-equation ))
      ))
  )

(def-reading-rule (math-array simple)
  "Simple reading rule for arrays. "
  (afl:new-block
    (afl:set-pronunciation-mode :math)
    (let
        ((contents
           (if *transpose-table*
               (transpose-table (contents math-array))
               (contents math-array ))))
      (loop
        for row in   contents do
          (afl:new-block
            (afl:new-block
              (afl:with-surrounding-pause *math-surround*
                (read-aloud (first row ))))
            (loop
              for column in (rest row) do
                (when column
                  (afl:new-block        ;dummy
                    (read-aloud  column ))
                  )                     ;end when
              )))                   ;done reading row
      )                                  ;Done reading all rows.
    ))

;;; Making the same changes as to the reading rule for arrays.

(def-reading-rule (tabular simple)
  "Simple reading rule for arrays. "
  (afl:new-block
    (afl:set-pronunciation-mode :text)
    (let
        ((contents
           (if *transpose-table*
               (transpose-table (contents
                                 tabular))
               (contents tabular ))))
      (loop
        for row in   contents do
          (afl:new-block
            (afl:with-surrounding-pause *math-surround*
              (read-aloud (first row))
              (loop
                for column in (rest row) do
                  (when column
                    (afl:new-block      ;dummy
                      (read-aloud  column ))
                    )                   ;end when
                )))                 ;done reading row
        )                            ;Done reading all rows.
      )))

(def-reading-rule (tabular quiet) nil)

(def-reading-rule (square-root simple)
  "Simple reading rule for square-root "
  (read-aloud " square root ")
  (cond
    ((and (math-object-subtype-p (argument square-root 1))
          (leaf-p (argument square-root 1 ))
          (null (attributes  (argument square-root 1 ))))
     (read-aloud (argument square-root 1 )))
    (t(read-aloud " of ")
      (with-reading-state (reading-state 'children)
        (read-aloud (argument square-root 1 ))))
    )
  )

(def-reading-rule (overbrace simple)
  (with-reading-state (reading-state 'overbrace)
    (read-aloud (argument overbrace 1 )))
  (read-attributes overbrace)
  )
(define-reading-state 'overbrace
    #'(lambda(state)
        (afl:multi-move-to state
                                        ;'(afl:left-volume 0)
                           ;'(afl:right-volume 75 )
                           ))
  )

(def-reading-rule (overline simple)
  (with-reading-state (reading-state 'overline)
    (read-aloud (argument overline 1 )))
  )

(def-reading-rule   ( integral-delimiter  simple)
  "Simple reading rule  for object integral-delimiter "
  (read-aloud " d ")
  (read-aloud (argument integral-delimiter 1))
  (afl:comma-intonation)
  )

(def-reading-rule (factorial simple)
  "Reading rule for factorials"
  (read-aloud (contents factorial))
  (afl:comma-intonation)
  (read-aloud "factorial")
  (afl:comma-intonation)
  )
