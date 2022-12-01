;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)

(in-package :aster)

;;; Thu Dec 10 16:21:01 EST 1992
;;; This file contains reading rules for a style that will use
;;; descriptive readings as opposed to the simple reading style
;;; defined in <(math-reading-rules)>
;;; which uses a layout oriented reading.
 

(def-reading-rule (integral descriptive)
    "Descriptive reading rule for integrals"
  (let* 
      ((lower-limit (subscript integral ))
       (upper-limit (superscript integral ))
       (children (children integral))
       (pause-amount (compute-pause integral ))
       (variable-of-integration    (variable-of-integration
                                    integral)))
    (afl:with-surrounding-pause pause-amount
      (read-aloud " Integral ")
      (cond
        ((and lower-limit upper-limit) 
         (read-aloud "[)] from  ")
         (when variable-of-integration
           (read-aloud (children variable-of-integration))
           (read-aloud " equals" ))
         (read-aloud lower-limit)
         (afl:tts-silence 1)
         (read-aloud " to ")
         (read-aloud upper-limit)
         (afl:tts-silence 1))
        (lower-limit
         (when variable-of-integration
           (read-aloud " with respect to ")
           (read-aloud (children variable-of-integration )))
         (read-aloud " over, ")
         (read-aloud lower-limit ))
        (variable-of-integration
         (read-aloud "with respect to ")
         (read-aloud (children  variable-of-integration ))))
      (afl:tts-force)
      (read-aloud   (first children))
      (afl:tts-force)
      (read-aloud variable-of-integration)
      (afl:tts-queue "[_,]")
      )
    )
  )

(def-reading-rule (summation descriptive)
    "Descriptive reading for summations"
  (let
      ((lower-limit (subscript summation))
       (upper-limit (superscript summation ))
       (summand (children summation )))
    (cond
      ((and lower-limit  upper-limit)
       (read-aloud "summation for[\] ")
       (read-aloud lower-limit)
       (read-aloud "to ")
       (read-aloud upper-limit))
      ( lower-limit
       (read-aloud "summation over, ")
       (read-aloud lower-limit)))
    (afl:tts-force)
    (read-aloud   summand)
    )
  )

(def-reading-rule (summation long-descriptive)
    "Descriptive reading for summations"
  (let
      ((lower-limit (subscript summation))
       (upper-limit (superscript summation ))
       (summand (children summation )))
    (cond
      ((and lower-limit  upper-limit)
       (read-aloud "summation for[\] ")
       (read-aloud lower-limit)
       (read-aloud "to ")
       (read-aloud upper-limit))
      ( lower-limit
       (read-aloud "summation over, ")
       (read-aloud lower-limit )))
    (afl:tts-force)
    (read-aloud "The quantity being summed is, ")
    (read-aloud   summand)
    )
  )

(def-reading-rule (integral  long-descriptive) 
    "long Descriptive reading rule for integrals"
  (let* 
      ((lower-limit (subscript integral ))
       (upper-limit (superscript integral ))
       (children (children integral))
       (variable-of-integration   (when
                                      (and (listp children)
                                           (typep (second children )
                                                  'integral-d))
                                    (second  children ))))
    (read-aloud " Integral ")
    (when variable-of-integration
      (read-aloud " with respect to ")
      (read-aloud (children variable-of-integration)))
    (cond
      ((and lower-limit upper-limit) 
       (afl:tts-queue "[_,] [/]with lower limit, ")
       (with-reading-state (reading-state 'subscript) 
         (afl:tts-queue lower-limit))
       (afl:tts-queue " [_,] and upper limit [_,] ")
       (with-reading-state (reading-state 'superscript) 
         (read-aloud upper-limit)))
      (lower-limit
       (read-aloud " over, ")
       (with-reading-state (reading-state 'subscript) 
         (read-aloud lower-limit ))))
    (afl:tts-force)
    (read-aloud "the expression being integrated is, ")
    (with-reading-state (reading-state 'children)
      (read-aloud   (first children)))
    (afl:tts-force)
    (read-aloud (second children))
    )
  )

;;; Modified: Mon Jan 11 17:18:11 EST 1993
;;; Most of the work now done in special patterns.
;;; <(Backing up old version.)>
;;; Eventually might go away completely.

  ;;; Variable: *EXPONENT-START*                               Author: raman
  ;;; Created: Sat Feb  6 16:35:59 1993

(defvar *exponent-start*
  "raised  to  "
  "String spoken before superscripts that are exponents. ")

(def-reading-rule (superscript descriptive)
    "Descriptive reading for superscripts, interprets as exponent"
  (let
      ((contents (contents superscript ))
       (parent (parent superscript )))
    (cond
      ((and (expression-p parent)
            (exponent-p contents))
       (afl:tts-queue "[_,]")
       (afl:low-intonation)
       (afl:tts-silence 5 )
       (read-aloud *exponent-start* )
       (afl:tts-queue "[_,]")
       (if (and
            (not (attribute-p (parent parent )))
           (= 1 (weight contents )))
           (read-aloud contents )
           (reading-rule superscript 'default  )))
      ( (and  (= 1 (weight contents ))
              (null (subscript parent )))
       (read-aloud contents )); to read x^*
      (t (reading-rule superscript 'default  )) 
      )
    )
  )



(defmethod exponent-p ((unary-minus unary-minus))
  t)
  ;;; Method: EXPONENT-P                                       Author: raman
  ;;; Created: Sat Dec 26 12:25:47 1992

(defmethod exponent-p ((parenthesis parenthesis))
  "Is this an exponent?"
  t
  )
  ;;; Method: EXPONENT-P                                       Author: raman
  ;;; Created: Fri Dec 25 17:12:54 1992

(defmethod exponent-p ((ordinary ordinary))
  "this is an exponent"
  t
  )

  ;;; Method: EXPONENT-P                                       Author: raman
  ;;; Created: Fri Dec 25 17:13:22 1992

(defmethod exponent-p ((binary-operator binary-operator))
  "Is this an exponent?"
  (children binary-operator)
  )


  ;;; Method: EXPONENT-P                                       Author: raman
  ;;; Created: Fri Dec 25 17:15:03 1992

(defmethod exponent-p ((math-subformula math-subformula))
  "Are its contents a valid exponent?"
  (exponent-p (contents math-subformula ))
  )

(defmethod exponent-p ((mathematical-function-name
                        mathematical-function-name ))
  "math functions are valid exponents "
  t)
(defmethod exponent-p ((fraction fraction ))
  "Fractions are valid exponents"
  t)

(defmethod exponent-p ((object t))
  "In general things are not valid exponents" nil)
(def-reading-rule (superscript default)
    "Default reading rule for superscript"
  (with-reading-state (reading-state 'superscript)
    (read-aloud (contents  superscript )))
  )

  ;;; Method: EXPRESSION-P                                     Author: raman
  ;;; Created: Tue Dec 22 09:04:09 1992
(defmethod expression-p ((object t)) nil)

(defmethod expression-p ((math-object math-object))
  "Check if this is an expression that can be exponentiated. "
  (or  (ordinary-p math-object)
       (mathematical-function-name-subtype-p math-object)
       (math-subformula-p math-object)
       (typep  math-object 'parenthesis ))
  )



(def-reading-rule  (ordinary descriptive)
    "Descriptive reading rule for ordinary "
  (let
      ((contents (contents ordinary ))
       (subscript (subscript ordinary ))
       (superscript (superscript ordinary)))
    (cond
      ((and  subscript superscript 
             (equal contents "D" ))
       (reading-rule ordinary 'derivative ))
      (t  (reading-rule ordinary 'simple )))))


(def-reading-rule (ordinary derivative )
    "This is a derivative, read it"
  (let
      ((pause-amount (compute-pause ordinary ))
       (superscript (superscript ordinary ))
       (subscript (subscript ordinary )))
    (cond
      ((index-variable-p subscript)
       (reading-rule ordinary 'simple))
      (t
       (when
           (= 2 (length (children (parent ordinary ))))
         (setf (pattern (parent ordinary )) 'derivative ))
         (afl:with-surrounding-pause pause-amount 
           (when  superscript
             (read-aloud (cardinal-number superscript )))
           (afl:tts-queue "derivative ")
           (when subscript
             (afl:tts-queue  "with respect  to ")
             (read-aloud subscript )
             (afl:tts-queue "of")
             (afl:comma-intonation)))))))

  ;;; Function: INDEX-VARIABLE-P                               Author: raman
  ;;; Created: Fri Dec 25 17:04:44 1992

(defun index-variable-p (ordinary) 
  "Is this an index variable?"
  (and (ordinary-p ordinary)
       (member (contents ordinary) '("i" "j" "k""l" "m" "n"))))

(def-reading-rule  (subscript descriptive) 
    "Descriptive reading rule for subscript "
  (let  ((contents (contents   subscript ))
         (parent (parent subscript )))
    (cond
      ((and  (ordinary-p  parent )
             (not (attribute-p (parent parent )))
             (= 1 (weight subscript  )))
       (read-aloud contents )
       (afl:tts-queue "[_,]"))
      (t (with-reading-state (reading-state 'subscript )
           (afl:comma-intonation)
           (read-aloud contents )))
      )
    )
  )

(def-reading-rule  (subscript long-descriptive) 
    "Long Descriptive reading rule for subscript "
  (let  ((contents (contents   subscript ))
         (parent (parent subscript )))
    (cond
      ((and  (ordinary-p  parent ) 
             (= 1 (weight subscript  )))
       (read-aloud "sub ")
       (read-aloud contents )
       (afl:tts-queue "[_,]"))
      (t (with-reading-state (reading-state 'subscript )
           (read-aloud "sub ")
           (read-aloud contents )))
      )
    )
  )

(def-reading-rule (a-log descriptive)
    (let* 
        ((subscript (subscript  a-log ))
         (children (children a-log )) 
         (attributes (attributes a-log ))
         (remaining-attributes (remove 'subscript  attributes
                                       :key #'attribute-name )))
      (afl:tts-queue  "log ")
      (mapc #'read-aloud  (sorted-attributes remaining-attributes ))
      (loop for child in children
            do (read-math-child  child ))
      (when subscript
        (afl:tts-queue "[_,]")
        (afl:tts-silence 1)
        (read-aloud "to the base ")
        (afl:comma-intonation)
        (read-aloud subscript ))
      )
  )

(def-reading-rule (a-log read-base-first)
    "Read base first if possible, eg log base a of x"
  (let* 
      ((subscript (subscript  a-log ))
       (children (children a-log )) 
       (attributes (attributes a-log ))
       (remaining-attributes (remove 'subscript  attributes :key #'attribute-name )))
    (afl:tts-queue "log ")
    (cond
      (remaining-attributes
       (mapc #'read-aloud  (sorted-attributes remaining-attributes )))
      (t (when subscript
           (afl:tts-queue " base ")
           (afl:comma-intonation)
           (read-aloud subscript )
           (afl:tts-queue "of")
           (afl:comma-intonation ))))
    (loop for child in children do (read-math-child  child ))
    (when  remaining-attributes
      (when subscript
        (afl:comma-intonation)
        (afl:tts-silence 1)
        (afl:tts-queue "to the base ")
        (afl:comma-intonation)
        (read-aloud subscript )))))

(def-reading-rule (a-log alternative)
    (let* 
        ((subscript (subscript  a-log ))
         (children (children a-log )) 
         (attributes (attributes a-log ))
         (remaining-attributes (remove 'subscript  attributes
                                       :key #'attribute-name )))
      (cond
        ((null remaining-attributes )
         (read-aloud "log")
         (when subscript
           (afl:tts-queue "[_,]")
                                        ;           (afl:tts-silence 1)
           (read-aloud " to the base ")
           (afl:low-intonation)
           (afl:tts-queue "[_,]")
           (read-aloud subscript )
           (read-aloud "of")
           (afl:comma-intonation))
         (mapc #'read-math-child children))   
        (t (read-aloud "log ")
           (mapc #'read-aloud  (sorted-attributes remaining-attributes ))
           (loop for child in children
                 do (read-math-child  child ))
           (when subscript
             (afl:tts-queue "[_,]")
             (afl:tts-silence 1)
             (read-aloud "to the base ")
             (afl:tts-queue "[_,]")
             (read-aloud subscript )))
        )
      )
  )

(def-reading-rule (a-sin alternative )
    "Alternative reading rule for a-sin says arcsin for a-sin inverse"
  (let*
      ((attributes (attributes a-sin))
       (superscript-pattern  (superscript-pattern a-sin))
       (children (children a-sin ))
       (remaining-attributes (remove 'superscript   attributes
                                     :key #'attribute-name )))
    (cond
      ((equal 'function-inverse superscript-pattern)
       (read-aloud "arcsine ")
       (mapc #'read-aloud remaining-attributes )
       (mapc #'read-math-child children ))
      (t (reading-rule a-sin 'simple ))
      )
    )
  )




(def-reading-rule (sub-group  concatenate)
    "descriptive reading rule for subgroups. "
  (let ((children (children sub-group )))
    (cond
      ((= 2 (length children ))
       (read-math-child (first children))
       (read-aloud "sub-group")
       (read-aloud (second children)))
      ((>  (length children ) 2)
       (loop for tail on children
             do
             (when (>= (length tail) 2) 
               (read-math-child (first tail) )
               (read-aloud " sub-group ")
               (read-aloud (second tail ))
               (afl:comma-intonation))))
      (t (error "Sub-group has less than two children. "))
      )
    )
  )
