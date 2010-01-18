;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;
;;;   ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Thu Oct 29 11:56:50 EST 1992
;;; Contains code for converting  output of process-math which is
;;; currently a stream of math objects to a quasi prefix form. 

;;; Variable: *OPERATOR-CLASSES*                             Author: raman
;;; Created: Thu Oct 29 14:10:50 1992

(defvar *operator-classes*
  '(arrow-operator
    relational-operator
    binary-operator
    big-operator
    quantifier
    mathematical-function-name
    negation-operator)
  "These classes are operators. ")

;;; Method: OPERATOR?                                        Author: raman
;;; Created: Thu Oct 29 14:09:22 1992

(defmethod operator?  ((math-object math-object))
  "Check if this is an operator"
  (some #'(lambda(type) (typep math-object  type)) *operator-classes*)
  )



;;; Variable: *MINUS*                                        Author: raman
;;; Created: Wed Nov  4 11:25:22 1992

(defvar *minus* "-" "Minus sign")
;;; Method: MINUS?                                           Author: raman
;;; Created: Wed Nov  4 11:23:23 1992

(defmethod minus? ((math-object math-object))
  "Is this  a minus sign?"
  (and (operator? math-object)
       (equal *minus* (contents math-object )))
  )

;;; recognize factorial symbol:

  ;;; Method: FACTORIAL-SYMBOL-P                               Author: raman
  ;;; Created: Sun Feb  7 10:00:17 1993

(defmethod factorial-symbol-p ((ordinary ordinary))
  "Is this a factorial symbol"
  (and (equal "!" (contents ordinary ))
       (null (children ordinary ))
       (null (attributes ordinary )))
  )

(defmethod factorial-symbol-p ((object t))
  "Things are not factorial symbols in general"
  nil)

;;; for the present anything that is not an operator is an operand.
;;; This means that punctuations in math become operands for the
;;; present, fix this later.


;;; Method: OPERAND?                                         Author: raman
;;; Created: Thu Oct 29 14:18:32 1992

(defmethod operand? ((math-object math-object))
  "Tests if math object is an operand"
  (not (operator? math-object))
  )



;;; Variable: *UNARY-OPERATORS*                              Author: raman
;;; Created: Thu Oct 29 14:25:41 1992

(defvar *unary-operators*
  '(quantifier big-operator mathematical-function-name
    negation-operator unary-minus)
  "These are unary operators")

;;; Method: UNARY-OPERATOR?                                  Author: raman
;;; Created: Thu Oct 29 14:24:19 1992

(defmethod unary-operator? ((math-object math-object))
  "Checks if this is a unary operator. "
  (some #'(lambda(type) (typep math-object type))  *unary-operators*)
  )

;;; Variable: *BINARY-OPERATORS*                             Author: raman
;;; Created: Thu Oct 29 14:29:14 1992

(defvar *binary-operators*
  '(arrow-operator relational-operator binary-operator)
  "These are binary operators")

;;; Method: BINARY-OPERATOR?                                 Author: raman
;;; Created: Thu Oct 29 14:28:09 1992

(defmethod binary-operator? ((math-object math-object))
  "Checks if this is a binary operator"
  (some #'(lambda(type) (typep math-object type)) *binary-operators*)
  )

;;; Following three functions  are based on chapter 32 of Winston and
;;; Horn's book on common lisp.

;;; This case is simple  since  we have a list of math objects
;;; representing the infix form and no nested lists.
;;; Base case is thus math-object not atom.
;;; For the present operators are either unary or binary
;;; Thu Oct 29 15:41:17 EST 1992


(defun inf-to-pre (infix-expression)
  "Convert from infix to prefix"
  (cond
    ((math-object-subtype-p  infix-expression)
     infix-expression)
    ((and (listp infix-expression)
          (= 1 (length infix-expression)))
     (first infix-expression))
    ((listp infix-expression)
     (process-term infix-expression nil nil)) ;Start with empty
                                        ;stacks.
    (t (error "Did not expect this "))
    )               
  )

;;; Modified: ;;; Sun Nov  1 10:24:52 EST 1992Sun Nov  1 10:24:53 EST 1992
;;; If an operator is followed by another operator this operator is
;;; probably unary. So when process-term sees an operator instead of
;;; an operand it should do the right thing. Things are complicated by
;;; the precedence rules for instance consider x+\sum... = ...  Now
;;; the precedence rules do not make sense.
;;; This is because + has higher precedence than the \sum and will
;;; therefore  be wrongly applied. 
;;; In this case for the
;;; present use the following strategy: If you see a unary operator,
;;; look ahead in the infix expression to see if there is an operator
;;; of lower precedence than the unary operator. If not then the case
;;; is simple ie: something like x+\sum ... So we can simply do a
;;; process-operator with infix expression as nil, the current
;;; operator stack and the result of parsing the remaining infix
;;; expression pushed on top of the current operand stack.
;;; In the more complicated case, ie: where there is an operator of
;;; lower precedence than the unary operator clip of the part of the
;;; infix expression upto that point and proceed as before.
;;; Above comments do not address the base case of the recursion. 

(defun process-term (infix-expression operators operands)
  "Set up recursion,  element in front of list is an operand."
  (cond
    ((endp infix-expression)            ;something went wrong:
     (process-operator infix-expression ; probably invalid expression
                       operators  operands)) ; try and recover
    ((minus? (first infix-expression))
     (process-unary-minus infix-expression
                          operators
                          operands))
    ((unary-operator? (first infix-expression))
     (process-unary-operator infix-expression
                             operators
                             operands))
    ((and (second infix-expression) 
          (unary-operator?
           (second infix-expression ))
          (not
           (mathematical-function-name-p
            (first operators)))) 
     (process-operator
      (cons (make-instance 'juxtaposition
                           :contents "juxtaposition")
            (rest infix-expression))
      operators
      (cons (first infix-expression )
            operands)))
    (t (process-operator (rest infix-expression)
                         operators
                         (cons (first infix-expression) ; no recursion
                               operands ))))
  )


;;; Function: PROCESS-UNARY-OPERATOR                         Author: raman
;;; Created: Sun Nov  1 10:31:14 1992

(defun process-unary-operator (infix-expression operators operands) 
  "Process unary operator"
  (assert (unary-operator? (first infix-expression)) nil
          "~a is not an unary operator: "
          (first infix-expression))
  (cond
    ((not (or (null operators)
              (binary-operator? (first operators ))
              (unary-minus-p (first operators ))))
                                        ;Unary operator  processed as  usual 
     (process-operator infix-expression
                       operators
                       operands))
    ((negation-operator-p (first infix-expression))
     (process-operator infix-expression
                       operators
                       operands))
    ((or
      (quantifier-p (first infix-expression))
      (big-operator-p (first infix-expression )))
     (process-big-unary-operator infix-expression
                                 operators operands))
    ((mathematical-function-name-subtype-p  (first infix-expression))
     (process-unary-function-operator  infix-expression
                                       operators operands))
    (t (error "Unknown unary operator ~a: "
              (first infix-expression ))))
  )


;;; Function: PROCESS-UNARY-MINUS                            Author: raman
;;; Created: Wed Nov  4 09:55:35 1992
(proclaim '(inline process-unary-minus))
(defun process-unary-minus (infix-expression operators operands) 
  "Process unary minus"
  (process-term (rest infix-expression)
                (cons (make-instance 'unary-minus
                                     :contents 'negative)
                      operators)
                operands)
  
  )

;;; Function: PROCESS-BIG-UNARY-OPERATOR                   Author: raman
;;; Created: Sun Nov  1 13:53:23 1992

(defun process-big-unary-operator (infix-expression  operators operands ) 
  "Process big unary operators like summations"
  (let* ((current-operator (first infix-expression))
         (lower-precedence-position
          (loop for term in (rest infix-expression) 
                and
                position = 1  then (+ 1 position)
                thereis (when
                            (and (operator? term)
                                 (precedence-< term 
                                               current-operator))  position )))
         )                              ; found a lower precedence operator
    (cond
      (lower-precedence-position        ;lower operator was foundhere:
       (let
           ((sub-expression
             (loop for index from 1 to lower-precedence-position
                   collect (pop infix-expression ))))
                                        ;infix-expression  holds rest of  input
         (process-operator  infix-expression
                            operators
                            (cons       ;subexpression 
                             (process-operator sub-expression
                                               nil
                                               nil)
                             operands ))))
      (t (process-operator              ;simple case
          nil
          operators
          (cons
           (process-operator infix-expression
                             nil
                             nil)
           operands))))
    )
  )


;;; Function: PROCESS-UNARY-FUNCTION-OPERATOR                Author: raman
;;; Created: Sun Nov  1 17:00:23 1992

(defun process-unary-function-operator (infix-expression operators operands) 
  "Process mathematical function as a unary operator. "
  (let* ((current-operator (first infix-expression))
         (original-infix-expression infix-expression)
         (lower-precedence-position
          (loop for term in (rest infix-expression)
                and
                position = 1 then (+ 1 position)
                thereis (when
                            (and (operator? term)
                                 (precedence-<= term 
                                                current-operator))  position )))
         )                              ; found a lower precedence operator
    (cond
      (lower-precedence-position        ;lower operator was foundhere:
       (let
           ((sub-expression
             (loop for index from 1 to lower-precedence-position
                   collect (pop infix-expression ))))
                                        ;infix-expression  holds rest of  input
         (cond                          ; check for base case
           ((and lower-precedence-position
                 (notany #'(lambda(term)
                             (operand? term))
                         sub-expression))
            (process-operator nil
                              operators 
                              (cons
                               (process-operator original-infix-expression
                                                 nil
                                                 nil)
                               operands )))
           ((and lower-precedence-position
                 (mathematical-function-name-subtype-p
                  (first infix-expression )))
            (introduce-juxtaposition-and-continue   infix-expression
                                                    operators
                                                    (cons ;subexpression 
                                                     (process-operator sub-expression
                                                                       nil
                                                                       nil)
                                                     operands )))
           (t (process-operator   infix-expression
                                  operators
                                  (cons ;subexpression 
                                   (process-operator sub-expression
                                                     nil
                                                     nil)
                                   operands ))))))
      (t
       (process-operator                ;simple case
        nil
        operators
        (cons
         (process-operator original-infix-expression
                           nil
                           nil)
         operands )))))
  )




;;; Variable: *INT*                                          Author: raman
;;; Created: Tue Nov  3 13:07:15 1992

(defvar *int* "int"  "Integral operator")

;;; Method: INTEGRAL-P                                       Author: raman
;;; Created: Tue Nov  3 13:05:48 1992
;;; Modified: Wed Dec  9 13:56:28 EST 1992
;;; Updated to use new integral class.
;;; Just need to check the class, *int* no longer used
(defmethod integral-p ((math-object math-object))
  "Checks if this is an integral operator"
  (typep math-object 'integral)
  )

(defun process-operator (infix-expression operators operands)
  "Iterate down infix-expression first element is now an operator."
  (cond
    ((and (not (endp infix-expression))
          (integral-d-subtype-p (first infix-expression )))
     (pop-integral-and-continue infix-expression
                                operators
                                operands))
    ((and (not (endp infix-expression))
          (factorial-symbol-p (first infix-expression )))
     (make-factorial-and-continue infix-expression
                                  operators
                                  operands))
    ((and (not (endp infix-expression))
          (operand?  (first infix-expression )))
     (introduce-juxtaposition-and-continue infix-expression
                                           operators
                                           operands))
    ((and  (endp infix-expression)
           (endp operators))          ;Termination?
     (return-quasi-prefix-form operands)) ;Result.
    ((and
      (not (endp infix-expression))   ;Not end of INFIX-EXPRESSION?
      (push-operator? (first infix-expression) operators operands ))
     (push-operator-and-continue infix-expression
                                 operators
                                 operands))
    (t (pop-operator-and-continue infix-expression
                                  operators
                                  operands )))
  )


;;; Following helping functions are declared inline. They are
;;; functions  only to make the top level functions readable.

(proclaim '(inline return-quasi-prefix-form)) 
(defun return-quasi-prefix-form  (operands) 
  "Return final quasi prefix form off operand stack"
  (if (= 1 (length operands))
      (first operands)
      (make-instance  'juxtaposition
                      :contents "juxtaposition" 
                      :children (reverse operands )))
  )


(proclaim '(inline introduce-juxtaposition-and-continue))
(defun introduce-juxtaposition-and-continue (infix-expression operators operands) 
  "Introduce juxtaposition and continue"
  (process-operator (cons             ;introduce juxtaposition
                     (make-instance 'juxtaposition
                                    :contents "juxtaposition")
                     infix-expression)
                    operators 
                    operands)
  )



(proclaim '(inline push-operator-and-continue))
(defun push-operator-and-continue (infix-expression operators operands) 
  "Push operator and continue"
  (process-term (rest infix-expression)
                (push-operator
                 (first infix-expression)
                 operators)   
                operands)
  )


(defun add-children (math-object children) 
  "Add chilldren to math object "
  (setf (children math-object)
        children )
  math-object)

(proclaim '(inline  pop-operator-and-continue)) 
(defun pop-operator-and-continue  (infix-expression operators operands) 
  "Pop operator and continue"
  (let*                               ;operator not pushed
      ( ( current-operator  (first operators))
       (arg-count (number-of-arguments current-operator )))
    (process-operator infix-expression
                      (rest operators) ;Pop operator,
                      (cons
                       (add-children   current-operator 
                                       (reverse ;children 
                                        (loop for i from 1 to
                                              arg-count
                                              when operands 
                                              collect  ( pop operands )))
                                       )
                       operands )))
  )

;;; Function: POP-INTEGRAL-AND-CONTINUE                      Author: raman
;;; Created: Tue Nov  3 14:54:21 1992

(defun pop-integral-and-continue (infix-expression operators operands) 
  "Pop integral off stack and continue"
  (cond
    ((notany #'integral-p operators)  ; not match anything
     (introduce-juxtaposition-and-continue infix-expression
                                           operators
                                           operands))
    ((integral-p (first operators))
     (increment-n-args (first operators) )
     (pop-operator-and-continue
      (rest infix-expression)
      operators
      (cons (first infix-expression)
            operands )))
    (t (pop-operator-and-continue infix-expression
                                  operators
                                  operands)))
  )


  ;;; Function: MAKE-FACTORIAL-AND-CONTINUE                    Author: raman
  ;;; Created: Sun Feb  7 10:04:43 1993

(defun make-factorial-and-continue (infix-expression operators operands ) 
  "Make a factorial object and continue "
  (let
      ((factorial-object(if operands
                            (first operands ))))
    (process-operator  (rest infix-expression)
                       operators
                       (cons  (make-instance 'factorial
                                             :contents factorial-object)
                              (rest operands )))
    )
  )


  ;;; Method: MINUS-P                                          Author: raman
  ;;; Created: Wed Apr 28 20:25:30 1993

(defmethod minus-p ((binary-operator binary-operator))
  "Is this a minus sign?"
  (equal "-" (contents binary-operator))
  )

(defmethod minus-p ((object t))
  "nothing else is a minus sign"
  nil)

;;; Function: PUSH-OPERATOR?                                 Author: raman
;;; Created: Mon Nov  2 12:38:51 1992
;;; Modified: Wed Apr 28 20:19:14 EDT 1993
;;; Handling fact that "-" is right associative.
;;; <(Backed up old version. )>

(defun push-operator? (incoming operator-stack operand-stack)
  "Decide whether to push operator"
  (or  (endp operator-stack)            ;empty stack?
       (endp operand-stack)
       (cond                            ; third disjunct
         ((mathematical-function-name-p incoming)
          (precedence-> incoming
                        (first operator-stack ))) ; functions  associate right
         ((minus-p (first operator-stack))
          (precedence->  incoming
                         (first operator-stack )))
         (t (precedence->= incoming
                           (first operator-stack )))))
  )


;;; Function: NUMBER-OF-ARGUMENTS                                Author: raman
;;; Created: Thu Oct 29 16:09:24 1992

(defun number-of-arguments (operator) 
  "Return number of arguments this operator takes"
  (cond
    ((big-operator-p operator) (operator-n-args operator))
    ((unary-operator? operator) 1)
    ((binary-operator?  operator) (operator-n-args operator))
    (t (error "Do not know how many args ~a takes: "
              operator )))
  )



;;; Method: SAME-OPERATOR?                                   Author: raman
;;; Created: Sat Oct 31 13:52:21 1992

(defmethod same-operator? ((operator-1 math-object)
                           (operator-2 math-object)) 
  "Tests if we have the same operator:"
  (and
   (equal (class-name (class-of  operator-1))
          (class-name (class-of operator-2)))
   (equal (contents operator-1)
          (contents operator-2))
   (same-attributes?   (attributes operator-1)
                       (attributes operator-2))
   )
  )


;;; Function: SAME-ATTRIBUTES?                               Author: raman
;;; Created: Sat Oct 31 13:58:03 1992

(defun same-attributes? (attr-list-1 attr-list-2) 
  "Checks if the attributes in the lists have the same values"
  (when
      (= (length attr-list-1 )
         (length attr-list-2))
    
    (loop for attr in *all-attributes*
          always  (same-attribute?
                   (find attr attr-list-1 :key  #'attribute-name)
                   (find attr attr-list-2 :key #'attribute-name))
          ))
  )

;;; Function: SAME-ATTRIBUTE?                                Author: raman
;;; Created: Sat Oct 31 14:14:15 1992

(defun same-attribute? (attr-1 attr-2) 
  "Checks if these have same attribute value"
  (or (and
       (attribute-p attr-1)
       (attribute-p attr-2)
       (same-math-object?  (attribute-value attr-1)
                           (attribute-value attr-2)))
      (equal attr-1 attr-2))
  )


;;; Function: PUSH-OPERATOR                                  Author: raman
;;; Created: Sat Oct 31 14:19:25 1992

(defun push-operator (operator operator-stack) 
  "Push operator on operator stack"
  (cond
    ((null operator-stack)  (push operator operator-stack))
    ((unary-operator? operator) (push operator operator-stack))
    ((and
      (binary-operator? operator) 
      (same-operator? operator
                      (first operator-stack))) ; do not push
     (increment-n-args (first operator-stack))
     operator-stack)
    (t (push operator
             operator-stack )))
  )


;;; Method: SAME-MATH-OBJECT?                                Author: raman
;;; Created: Sat Oct 31 19:47:10 1992

(defmethod same-math-object? ((object-1 math-object)
                              (object-2 math-object)) 
  "Checks if math objects are the same"
  (and
   (same-math-object?  (contents object-1)
                       (contents object-2))
   (same-attributes?
    (attributes object-1)
    (attributes object-2))
   )
  )

(defmethod same-math-object? ((object-1 list)
                              (object-2 list)) 
  "Checks if math objects are the same"
  (loop for o-1 in object-1
        and o-2 in object-2
        always (same-math-object? o-1 o-2))
  )

;;; Method: SAME-MATH-OBJECT?                                Author: raman
;;; Created: Sat Oct 31 19:49:59 1992

(defmethod same-math-object? (object-1 object-2) 
  "Default method for standard objects"
  (equal object-1 object-2) 
  )




