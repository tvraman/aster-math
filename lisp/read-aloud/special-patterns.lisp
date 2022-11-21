;;;   -*-   Mode: LISP -*-    ;;;
 
 

(in-package :aster)


;;; Sun Dec 27 07:56:14 EST 1992
;;; Implements code to allow the user to specify special patterns.
;;; This done by generating after methods for initialize-instance
;;; that check the created instance and if the instance matches a
;;; special pattern set the special-pattern slot.
;;; read-aloud can check this slot to see if a special pattern has
;;; been identified, and if in addition the special patterns reading
;;; style is active and a reading rule  called <special-pattern-value>
;;; has been defined for this object type, then that rule is called.
;;; Work out a clean way for specifying a number of special patterns
;;; to check for:
 


;;;
;;; <(backed up. )>
;;; Checking for special patterns at initialization time is buggy.
;;; Instead introduce a generic method special-pattern which gets
;;; called before the object is read, in the around method for
;;; read-aloud. If special-pattern returns a special pattern, then
;;; read-aloud will invoke the reading rule of that name. 
 


;;; Only math objects have special patterns for now:
;;; special-pattern is being memoized by adding a slot pattern to
;;; math-object. If later on other document objects  also need special
;;; patterns, then move this slot to the appropriate super class, and
;;; define the around method for that super class.
 

;;; Do nothing in the case of ordinary objects, just return nil. 
;;; around method not defined  in this case
;;; Moved to math-classes.lisp to avoid warning.



;;; Memoizing special-pattern
;;; Sun Feb  7 18:22:23 EST 1993
;;; Introducing ability to turn off special patterns selectively.
;;; Use a hash table to hold class names for which special patterns
;;; are active.
;;; This means that when a special pattern is written it will have to
;;; be explicitly activated.
;;; The hash table will hold the class name of the class for which the
;;; special pattern is being activated.


  ;;; Variable: *ACTIVE-SPECIAL-PATTERN*                       Author: raman
  ;;; Created: Sun Feb  7 18:24:21 1993

(defvar *active-special-pattern*
  (make-hash-table :test #'equal)
  "Holds class names of classes for which special pattern matching is
active")


  ;;; Function: TURN-ON-SPECIAL-PATTERN                        Author: raman
  ;;; Created: Sun Feb  7 18:25:42 1993

(defun turn-on-special-pattern (class-name)
  "Turn on special pattern matching for this class"
  (setf (gethash class-name *active-special-pattern*)  t)
  )

  ;;; Function: TURN-OFF-SPECIAL-PATTERNS                      Author: raman
  ;;; Created: Sun Feb  7 18:27:09 1993

(defun turn-off-special-pattern (class-name )
  "Turn off special pattern matching for this class"
  (setf (gethash class-name *active-special-pattern* ) nil)
  )


  ;;; Function: ACTIVE-SPECIAL-PATTERN-P                       Author: raman
  ;;; Created: Sun Feb  7 18:32:05 1993

(defun active-special-pattern-p (object) 
  "Check if special patterns active for this object"
  (let ((class-name (class-name (class-of object ))))
    (gethash class-name *active-special-pattern* )
    )
  )

;;; Modified: Sun Feb  7 18:27:52 EST 1993
;;; Check for special patterns only if activated

(defmethod special-pattern :around ((math-object math-object ))
           "Memoizing special pattern "
           (when (active-special-pattern-p math-object )
             (let ((pattern (pattern math-object )))
               (or pattern                ;already known
                   (setf  (pattern math-object )
                          (when (compute-applicable-methods
                                 #'special-pattern
                                 (list math-object ))
                            (call-next-method )) ; compute it
                          ) 
                   )
               ))
           )

;;; attributes have special pattern as well:

(defmethod special-pattern :around ((attribute attribute ))
           "Memoizing special pattern "
           (when (active-special-pattern-p attribute )
             (let ((pattern (pattern attribute )))
               (or pattern                ;already known
                   (setf  (pattern attribute )
                          (when (compute-applicable-methods
                                 #'special-pattern
                                 (list attribute ))
                            (call-next-method )) ; compute it
                          ) 
                   )
               ))
           )

;;; def-special-pattern is more cumbersome than writing the defmethod
;;; directly, so not using it for the present.



(defmethod  special-pattern ((fraction fraction ))
  "Check for special fractions"
  (let ((numerator (numerator-of fraction ))
        (denominator (denominator-of fraction )))
    (cond
      ((and 
        (number-1-p  numerator )
        (number-2-p  denominator))
       'half)
      ((and  (or  (leaf-p numerator)
                  (read-as-prefix? numerator))
             (number-2-p  denominator ))
       'one-half-of)
      ((and(or  (leaf-p numerator)
                (read-as-prefix? numerator))
           (math-number-p  denominator)
           (null (attributes denominator)))
       'one-over-n)
      )
    )
  )
(turn-on-special-pattern 'fraction )

(def-reading-rule (fraction half)
    "read one half"
  (read-aloud "half")
  )

(def-reading-rule (fraction one-over-n)
    "Read as one <cardinal-number n>"
  (read-aloud "one")
  (read-aloud (cardinal-number (denominator-of fraction )))
  (afl:tts-queue "[_,]")
  (when (not (number-1-p (numerator-of fraction )))
    (read-aloud (numerator-of fraction )))
  )


(def-reading-rule (fraction one-half-of)
    (read-aloud "one half ")
  (afl:tts-queue "[_,]")
  (read-aloud (numerator-of fraction ))
  )


;;; Interpreting special notation like D_x^1 as a derivative can be
;;; done either by writing a descriptive reading rule for ordinary
;;; objects  or by writing a special pattern. The advantage in writing
;;; a special pattern is that at the time of writing the reading rule,
;;; we neet not bother about saying how the object is to be read if
;;; the pattern does not match.

;;{{{ special pattern matching methods

(defmethod math-number-p  ((math-subformula math-subformula ))
  "Some math subformulas are plain math numbers"
  (and
   (null (attributes math-subformula ))
   (leaf-p math-subformula)
   (math-number-p (contents math-subformula )))
  )

  ;;; Method: NUMBER-1-P                                       Author: raman
  ;;; Created: Sun Dec 27 08:12:44 1992

(defmethod number-1-p ((math-number math-number))
  "Is this number 1?"
  (and (null (attributes math-number ))
       (equal "1" (contents  math-number )))
  )

(defmethod number-1-p ((math-subformula math-subformula ))
  (and
   (null (attributes  math-subformula ))
   (leaf-p math-subformula)
   (number-1-p (contents  math-subformula )))
  )

(defmethod number-1-p ((object t))
  nil)

  ;;; Method: CAPITAL-T-P                                      Author: raman
  ;;; Created: Mon Dec 21 17:50:28 1992

(defmethod capital-t-p ((ordinary ordinary))
  "Recognize cap t"
  (and
   (null (attributes ordinary ))
   (equal "T" (contents ordinary  )))
  )


  ;;; Method: CAPITAL-T-P                                      Author: raman
  ;;; Created: Mon Dec 21 17:51:32 1992

(defmethod capital-t-p ((math-subformula math-subformula))
  "Checks if this is cap t"
  (and
   (null (attributes math-subformula ))
   (leaf-p (contents math-subformula ))
   (capital-t-p (contents math-subformula )))
  )


  ;;; Method: NUMBER-2-P                                       Author: raman
  ;;; Created: Mon Dec 21 18:27:27 1992

(defmethod number-2-p ((ordinary ordinary))
  "Checks if the contents is the number 2. "
  (and
   (null (attributes ordinary ))
   (equal "2" (contents ordinary  )))
  )



  ;;; Method: NUMBER-2-P                                       Author: raman
  ;;; Created: Mon Dec 21 18:28:39 1992

(defmethod number-2-p ((math-subformula math-subformula))
  "Checks if this is the number 2. "
  (and
   (null (attributes math-subformula ))
   (leaf-p (contents math-subformula ))
   (number-2-p (contents math-subformula )))
  )


  ;;; Method: NUMBER-3-P                                       Author: raman
  ;;; Created: Mon Dec 31 18:37:37 1993

(defmethod number-3-p ((ordinary ordinary))
  "Checks if the contents is the number 3. "
  (and
   (null (attributes ordinary ))
   (equal "3" (contents ordinary  )))
  )



  ;;; Method: NUMBER-3-P                                       Author: raman
  ;;; Created: Mon Dec 31 18:38:39 1993

(defmethod number-3-p ((math-subformula math-subformula))
  "Checks if this is the number 3. "
  (and
   (null (attributes math-subformula ))
   (leaf-p (contents math-subformula ))
   (number-3-p (contents math-subformula )))
  )



(defmethod capital-t-p ((object t))
  nil)

(defmethod number-2-p ((object t))
  nil)

(defmethod number-3-p ((object t))
  nil)


  ;;; Method: MATH-COMMA-P                                     Author: raman
  ;;; Created: Fri Feb  5 12:07:49 1993

(defmethod math-comma-p ((ordinary ordinary))
  "Is this a comma?"
  (equal "," (contents ordinary ))
  )

(defmethod math-comma-p((object t))
  "nothing else is a comma."
  nil)

;;; recognize periods in math mode.

  ;;; Method: MATH-PERIOD-P                                    Author: raman
  ;;; Created: Mon Feb  8 20:43:53 1993

(defmethod math-period-p  ((ordinary ordinary))
  "Is this a period in math mode?"
  (and (equal "." (contents ordinary ))
       (null (attributes ordinary ))
       (null (children ordinary )))
  )

(defmethod math-period-p ((object t))
  "Things in general are not periods. "
  nil)

;;}}}
;;{{{ recognizing unary minus

  ;;; Method: SPECIAL-PATTERN                                  Author: raman
  ;;; Created: Thu Dec 31 10:11:14 1992

(defmethod special-pattern ((math-object math-object))
  "Check for balanced trees"
  (cond
    ((balanced-tree-p math-object) 'balanced-tree)))


;;; Detect inverse of functions:


  ;;; Method: UNARY-MINUS?                                     Author: raman
  ;;; Created: Sat Jan  2 09:56:17 1993

(defmethod unary-minus? ((unary-minus unary-minus))
  "Unary minus is clearly a unary minus!"
  t)


(defmethod unary-minus? ((object t))
  "Most things are not unary minuses"
  nil)

(defmethod unary-minus? ((math-subformula math-subformula ))
  "Check the contents"
  (and
   (unary-minus? (contents  math-subformula ))
   (null (attributes math-subformula )))
  )


  ;;; Method: NEGATED-OBJECT                                   Author: raman
  ;;; Created: Sat Jan  2 10:20:31 1993

(defmethod negated-object ((unary-minus unary-minus))
  "Return object that is being negated "
  (first (children unary-minus))
  )



  ;;; Method: NEGATED-OBJECT                                   Author: raman
  ;;; Created: Sat Jan  2 10:21:12 1993

(defmethod negated-object ((math-subformula math-subformula))
  "Return object negated "
  (when (unary-minus? math-subformula )
    (negated-object (contents math-subformula ))))

;;}}}
;;; Moving pattern matching for superscript here from descriptive
;;; reading rules. the descriptive reading rule might go away
;;; depending on which method proves more convenient.
(defmethod special-pattern ((superscript superscript ))
  "Recognize special patterns in the superscript"
  (let
      ((contents (contents superscript ))
       (parent (parent superscript )))
    (cond
      ((and(or  (expression-p parent)
                (brackets-p parent ))
           (capital-t-p contents )) 'transpose )
      ((and (expression-p parent)
            (number-2-p contents )) 'square)
      ((and (expression-p parent)
            (number-3-p  contents )) 'cube)
      ((and (mathematical-function-name-p parent)
            (unary-minus?  contents )
            (number-1-p (negated-object contents ))) 'function-inverse)
      ((and (expression-p parent)
            (exponent-p contents)) 'exponent )
      )                                 ; end cond
    
    )
  )
(turn-on-special-pattern 'superscript )

(def-reading-rule (superscript  function-inverse )
    (read-aloud "inverse ")
  )


(def-reading-rule (superscript  transpose )
    (read-aloud "transpose "))

(def-reading-rule (superscript square)
    (read-aloud "sqquare")
  )

(def-reading-rule (superscript cube )
    (read-aloud "cube" ))





  ;;; Method: SPECIAL-PATTERN                                  Author: raman
  ;;; Created: Sun Sep  5 15:51:56 1993

(defmethod special-pattern ((binary-operator binary-operator))
  "Special pattern for binary operators"
  (cond
    ((equal "-" (contents binary-operator)) 'difference)
    ((equal "+" (contents binary-operator)) 'sum)
    ((equal "*" (contents binary-operator)) 'product)
    ((equal "/" (contents binary-operator)) 'quotient)
    )
  )
(turn-on-special-pattern 'binary-operator)
  ;;; Method: SPECIAL-PATTERN                                  Author: raman
  ;;; Created: Fri Feb  5 11:04:22 1993

(defmethod special-pattern ((juxtaposition juxtaposition))
  "Check for special patterns in juxtaposition"
  (let
      ((children (children juxtaposition )))
    (if (and
         (> (length children) 2)
         (math-period-p
          (elt children  (- (length children ) 1))))
        (setf children  (butlast children )))
    (cond
      ((and  (= 2 (length children ))
             (leaf-p (first children ))
             (= 1 (weight (first children )))
             (parenthesis-p (second children ))) 'function-application)
      ((and (every #'expression-p children )
            (notany #'math-comma-p children ))
       'product)
      )
    )
  )
(turn-on-special-pattern 'juxtaposition )

(def-reading-rule (juxtaposition product)
    "read a juxtaposition that is a product"
  (let* 
      ((children (children juxtaposition ))
       (count (length children  )))
    (cond
      ((= 1 count) (read-aloud children))
      ((every #'leaf-p children ) (reading-rule juxtaposition 'simple))
      ((some  #'mathematical-function-name-p children )
       (reading-rule juxtaposition 'simple))
      ((= 2 count) (read-math-child (first children))
       (read-aloud "times")
       (read-math-child (second children )))
      ((and (> count 2)
            (every #'parenthesis-p children ))
       (read-aloud "the product  of the expressions, ")
       (mapc #'read-math-child children ))
      (t (read-aloud  "the product, ")
         (mapc #'read-math-child children ))
      )
    )
  )


