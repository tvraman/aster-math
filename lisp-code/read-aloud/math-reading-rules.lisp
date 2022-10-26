;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :cl-user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))



;;; This file contains reading rule definitions using the new
;;; defmethod reading-rule.
;;; Will eventually replace the reading rules in math-reading-rules.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-reading-rule (math-object simple) 
    "Simple reading rule for math objects "  
  (let* 
      ((pause-duration  (compute-pause math-object )))
    (afl:with-surrounding-pause pause-duration 
      (cond
        ((leaf-p math-object) (read-math-object-and-attributes math-object))
        ( (read-as-infix?  math-object) (read-as-infix math-object))
        ((read-as-prefix?  math-object) (read-as-prefix math-object))
        (t (error "Do not know what to do ~%"))
        )))
  )

(def-reading-rule (diagonal-dots simple) 
    "Reading rule for diagonal dots. "
  (afl:new-block
   (loop for i from 1 to 3 do 
         (afl:tts-queue "and so on")
         (afl:low-intonation)
         (afl:tts-force)
         (afl:local-set-state (afl:multi-step-by
                               afl:*current-speech-state*
                               '(afl:loudness -1)
                               '(afl:left-volume -1.25)
                               '(afl:right-volume -.75)))))
  )

(def-reading-rule (vertical-dots simple)
    "Simple reading rule for vertical dots. "
  (afl:new-block
   (loop for i from 1 to 3 do
         (afl:tts-queue "dot")
         (afl:low-intonation)
         (afl:local-set-state
          (afl:multi-step-by
           afl:*current-speech-state*
           '(afl:loudness -1)
           '(afl:left-volume -1.5)
           '(afl:right-volume -1.5)))))
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
      (read-aloud "fraction ")
      (afl:subclause-boundary))
    (afl:with-surrounding-pause pause-amount
      (cond
        ((and (leaf-p (numerator-of fraction))
              (leaf-p (denominator-of fraction )))
         (read-aloud (numerator-of fraction))
         (when (> (weight (numerator-of fraction )) 1)
           (afl:subclause-boundary))
         (read-aloud "over" ) 
         (read-aloud
          (denominator-of fraction ))
         (afl:comma-intonation))
        (t (with-reading-state (reading-state 'children)
             (read-aloud (numerator-of fraction )))
           (afl:subclause-boundary)
           (read-aloud  " divided by,  ")
           (with-reading-state (reading-state 'children)
             (read-aloud (denominator-of fraction )))
           ))
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



(define-reading-state 'left-hand-side
    #'(lambda(state)
        (afl:multi-move-to  state 
                            '(afl:left-volume 100)
                            '(afl:right-volume 0 )))
  )

(define-reading-state 'right-hand-side
    #'(lambda(state)
        (afl:multi-move-to state
                           '(afl:left-volume 0 )
                           '(afl:right-volume 100 )))
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
        (number (number math-equation ))
        (label-name  (when (label math-equation)
                       (label-name (label math-equation  )))))
    (afl:new-block
     (afl:local-set-state :math) 
     (afl:local-set-state (reading-state 'math ) )
     (read-aloud equation )
     (afl:pause 5)
     (if label-name 
         (read-aloud label-name)
         (read-aloud  (format nil "math equation ~a" number  )))
     (afl:tts-force )
     (relabel-if-necessary (label math-equation ))
     ))
  )


;;; Reimplementing following reading rules using move-by rather than
;;; modifying step size.  <(Compare this to the old version)>
;;; Modified: Sun Apr 11 08:37:47 EDT 1993
;;; Introducing a new block for each element to allow relative
;;; browsing.  <( Refer to old version for explanation)>
;;; Made more efficient:
;;; Begin a block for the array, and move to the left.
;;; when looping through arrays move to the right, and have a dummy
;;; block for reading the array element so that the right state gets
;;; recorded for relative browsing.
;;; read first element of each row and then loop, this will avoid
;;; having to check for the last element when stepping to the right.
;;; Make cleaner by using named reading states.
(define-reading-state 'array-start
    #'(lambda(state)
        (afl:multi-move-to state
                           '(afl:left-volume 100)
                           '(afl:right-volume 0)))
  )

(def-reading-rule (math-array simple)
    "Simple reading rule for arrays, uses directional audio"
  (afl:new-block
   (afl:local-set-state :math)
   (afl:local-set-state (reading-state 'array-start))
   (let
       ((contents  (if *transpose-table*
                       (transpose-table (contents
                                         math-array))
                       (contents math-array )))
        (left-offset nil)
        (right-offset nil))
     (loop for row in   contents do
           (afl:new-block
            (setf left-offset(* -1  (afl:length-of-subinterval
                                     'afl:left-volume (length row ))))
            (setf right-offset  (afl:length-of-subinterval
                                 'afl:right-volume (length row )))
            (afl:new-block
             (afl:with-surrounding-pause *math-surround* 
               (read-aloud (first row ))))
            (loop for column in (rest row) do 
                  (afl:local-set-state
                   (afl:multi-move-by
                    afl:*current-speech-state*
                    `(afl:left-volume ,left-offset )
                    `(afl:right-volume ,right-offset )))
                  (when column
                    (afl:new-block      ;dummy
                     (read-aloud  column ))
                    )                   ;end when 
                  )))                   ;done reading row
     )                                  ;Done reading all rows. 
   )
  )

;;; Making the same changes as to the reading rule for arrays. 

(def-reading-rule (tabular simple)
    "Simple reading rule for arrays, uses directional audio"
  (afl:new-block
   (afl:local-set-state :text)
   (afl:local-set-state (reading-state 'array-start))
   (let
       ((contents  (if *transpose-table*
                       (transpose-table (contents
                                         tabular))
                       (contents tabular )))
        (left-offset nil)
        (right-offset nil))
     (loop for row in   contents do
           (setf left-offset(* -1  (afl:length-of-subinterval
                                    'afl:left-volume (length row ))))
           (setf right-offset  (afl:length-of-subinterval
                                'afl:right-volume (length row )))
           (afl:new-block
            (afl:with-surrounding-pause *math-surround* 
              (read-aloud (first row))
              (loop for column in (rest row) do 
                    (afl:local-set-state
                     (afl:multi-move-by
                      afl:*current-speech-state*
                      `(afl:left-volume ,left-offset )
                      `(afl:right-volume ,right-offset )))
                    (when column
                      (afl:new-block    ;dummy
                       (read-aloud  column ))
                      )                 ;end when 
                    )))                 ;done reading row
           )                            ;Done reading all rows. 
     ))
  )

(def-reading-rule (tabular quiet) nil)



;;; Modified: Sun Apr 11 13:36:17 EDT 1993
;;; <(Fixed the bug that caused relative readings to fail. )>
(def-reading-rule (tabular bus-schedule) 
    "Simple reading rule for arrays, uses directional audio"
  (afl:new-block (afl:local-set-state :text)
                 (afl:local-set-state (reading-state 'array-start))
                 (let* 
                     ((contents  (if *transpose-table*
                                     (transpose-table (contents
                                                       tabular))
                                     (contents tabular )))
                      (left-offset nil)
                      (right-offset nil)
                      (headers (first contents )))
                   (loop for row in   (rest contents)  do
                         (setf left-offset(* -1  (afl:length-of-subinterval
                                                  'afl:left-volume (length row ))))
                         (setf right-offset  (afl:length-of-subinterval
                                              'afl:right-volume (length row )))
                         (afl:new-block
                          (afl:with-surrounding-pause *math-surround*
                            (read-aloud (first headers))
                            (afl:tts-queue " at ")
                            (read-aloud (first row))
                            (loop for column in (rest row)
                                  and head in (rest headers) do 
                                  (afl:local-set-state
                                   (afl:multi-move-by
                                    afl:*current-speech-state*
                                    `(afl:left-volume ,left-offset )
                                    `(afl:right-volume ,right-offset )))
                                  (when column
                                    (afl:new-block ;dummy
                                     (read-aloud  head)
                                     (afl:tts-queue " at ")
                                     (read-aloud  column ))
                                    )   ;end when 
                                  )))   ;done reading row
                         )              ;Done reading all rows. 
                   ))
  )

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
                           '(afl:left-volume 0)
                           '(afl:right-volume 75 )))
  )


(def-reading-rule (overline simple)
    (with-reading-state (reading-state 'overline)
      (read-aloud (argument overline 1 )))
  )

(def-reading-rule   ( integral-delimiter  simple)
    "Simple reading rule  for object integral-delimiter "
  (read-aloud " d ")
  (read-aloud (argument integral-delimiter 1))
  (afl:subclause-boundary)
  )


(def-reading-rule (factorial simple)
    "Reading rule for factorials"
  (read-aloud (contents factorial))
  (afl:subclause-boundary)
  (read-aloud "factorial")
  (afl:comma-intonation)
  )
