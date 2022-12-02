;;;   -*-   Mode: LISP -*-    ;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman
;;; All Rights Reserved
;;;


(in-package :aster)
 
;;{{{ attributes

;;; Class: ATTRIBUTE                                     Author: raman
;;; Created: Fri Sep 18 08:33:54 1992
;;; Class: ATTRIBUTE                                         Author: raman
;;; Created: Sat Dec 19 14:10:53 1992

(defclass attribute (math)
  ((name :initform nil :initarg :name :accessor attribute-name)
   (value :initform nil :initarg :value :accessor attribute-value
          :accessor contents)
   (pattern :initform nil :initarg pattern :accessor pattern )
   )
  (:documentation "An attribute in math"))

(defun make-attribute (&key name value parent)
  (assert
   (member name
           '(SUBSCRIPT SUPERSCRIPT ACCENT UNDERBAR
             LEFT-SUBSCRIPT LEFT-SUPERSCRIPT )) nil
          "~a is not a valid attribute name" name)
  (let ((self (make-instance name
                             :name name
                             :parent parent
                             :value value )))
    self))


(defun attribute-p (self)
  (typep self 'attribute))

(defclass subscript (attribute) () )
(defclass superscript  (attribute) () )
(defclass accent (attribute) () )
(defclass underbar (attribute) () )
(defclass left-subscript (attribute) () )
(defclass left-superscript  (attribute) () )

;;; Function: APPEND-ATTRIBUTE-VALUE                         Author: raman
;;; Created: Fri Sep 18 09:47:38 1992

(defun append-attribute-value ( existing-attribute additional-value)
  "append attribute value to an existing attribute"
  (setf (attribute-value existing-attribute)
        (cons   (attribute-value existing-attribute)
                (list additional-value )))
  existing-attribute
  )

;;}}}
;;{{{ math object

(defclass math-object (math)
  ((contents :initform nil :initarg  :contents
             :accessor contents :accessor math-object-contents)
   (children :initform nil
             :initarg :children
             :accessor children :accessor math-object-children)
   (pattern :initarg :pattern :initform nil
            :accessor pattern)
   (weight  :initform nil :initarg weight
            :accessor internal-weight)
   (substitution :initform nil :initarg :substitution
                 :accessor substitution )
   (type
    :initform nil
    :initarg  :type
    :accessor math-object-type)
   (attributes :initform nil
               :initarg :attributes
               :accessor attributes
               :accessor math-object-attributes)
   )
  (:documentation "A math object  with its visual annotations"))

(defun make-math-object ()
  (let ((self (make-instance 'math-object)))
    self))

(defun math-object-p (self)
  (typep self  'math-object))

;;; Function: MATH-OBJECT-SUBTYPE-P                          Author: raman
;;; Created: Wed Oct 14 16:08:23 1992

(defun math-object-subtype-p (self)
  "Check if arg is  a subtype of math-object"
  (typep self 'math-object)
  )
;;; Default method:

  ;;; Method: SUBSTITUTION                                     Author: raman
  ;;; Created: Sun May 23 13:17:28 1993

(defmethod substitution ((document document))
  "Only math objects have substitutions. "
  nil)

;;; Method: ADD-ATTRIBUTE                                    Author: raman
;;; Created: Fri Sep 18 08:44:18 1992
(defmethod add-attribute (attribute-name attribute-value
                          (math-object math-object))
  "add attribute to math object"
  (let* ((attribute-list (attributes  math-object ))
         (existing-attribute  (find
                               attribute-name attribute-list
                               :key #'attribute-name ) )
                                        ;attribute already exists
         )
    (cond
      (existing-attribute ;append to existing attribute
       (setf attribute-list
             (append  (remove existing-attribute
                              attribute-list )
                      (list (append-attribute-value  existing-attribute
                                                     attribute-value ))))
       (setf (attributes math-object)
             attribute-list)
       )
      (t ; new attribute
       (setf (attributes  math-object)
             (append   (attributes  math-object )
                       (list (make-attribute
                              :name attribute-name
                              :parent math-object
                              :value attribute-value )))))
      )
    math-object
    ))

;;}}}
;;{{{delimited expressions

;;; Class: DELIMITED-EXPRESSION                              Author: raman
;;; Created: Tue Dec  1 11:37:32 1992

(defclass delimited-expression (math-object)
  (
   (type
    :initform nil
    :initarg  :type
    :accessor delimited-expression-type)
   (open-delimiter :initform nil :initarg :open-delimiter
                   :accessor open-delimiter)
   (close-delimiter :initform nil :initarg :close-delimiter
                    :accessor close-delimiter))
  (:documentation "A delimited expression "))

(defun make-delimited-expression ()
  (let ((self (make-instance 'delimited-expression)))
    self))


(defun delimited-expression-p (self)
  (typep self 'delimited-expression))

;;}}}
;;{{{ Accessors for attributes

;;; Method: SUBSCRIPT                                        Author: raman
;;; Created: Tue Dec  8 12:04:42 1992

(defmethod subscript ((math-object math-object))
  "Return  Subscript if any. "
  (let ((subscript (find 'subscript (attributes math-object )
                         :key #'attribute-name )))
    (when subscript
      (attribute-value subscript))
    )
  )

(defmethod subscript ((document document ))
  "Default method: documents do not have subscripts"
  nil)

(defmethod superscript ((document document ))
  "Default method, documents do not have superscripts"
  nil)
(defmethod superscript ((math-object math-object))
  "Return  Superscript if any. "
  (let ((superscript (find 'superscript (attributes math-object )
                           :key #'attribute-name )))
    (when superscript
      (attribute-value superscript))
    )
  )

(defmethod accent ((math-object math-object))
  "Return  Accent if any. "
  (let ((accent (find 'accent (attributes math-object )
                      :key #'attribute-name )))
    (when accent
      (attribute-value accent))
    )
  )

(defmethod underbar ((math-object math-object))
  "Return  Underbar if any. "
  (let ((underbar (find 'underbar (attributes math-object )
                        :key #'attribute-name )))
    (when underbar
      (attribute-value underbar))
    )
  )

(defmethod left-subscript ((math-object math-object))
  "Return  Left-Subscript if any. "
  (let ((left-subscript (find 'left-subscript (attributes math-object )
                              :key #'attribute-name )))
    (when left-subscript
      (attribute-value left-subscript))
    )
  )

(defmethod left-superscript ((math-object math-object))
  "Return  Left-Superscript if any. "
  (let ((left-superscript (find 'left-superscript (attributes math-object )
                                :key #'attribute-name )))
    (when left-superscript
      (attribute-value left-superscript))
    )
  )
;;; default method:

(defmethod special-pattern ((object t)) nil)
(defmethod superscript-pattern ((math-object math-object))
  "Return  pattern for superscript if any "
  (let ((superscript (find 'superscript (attributes math-object )
                           :key #'attribute-name )))
    (when superscript
      (special-pattern  superscript))
    )
  )

(defmethod subscript-pattern ((math-object math-object))
  "Return  pattern for subscript if any "
  (let ((subscript (find 'subscript (attributes math-object )
                         :key #'attribute-name )))
    (when subscript
      (special-pattern  subscript))
    )
  )

(defmethod underbar-pattern ((math-object math-object))
  "Return  pattern for underbar if any "
  (let ((underbar (find 'underbar (attributes math-object )
                        :key #'attribute-name )))
    (when underbar
      (special-pattern  underbar))
    )
  )

(defmethod accent-pattern ((math-object math-object))
  "Return  pattern for accent if any "
  (let ((accent (find 'accent (attributes math-object )
                      :key #'attribute-name )))
    (when accent
      (special-pattern  accent))
    )
  )

(defmethod left-superscript-pattern ((math-object math-object))
  "Return  pattern for left-superscript if any "
  (let ((left-superscript (find 'left-superscript (attributes math-object )
                                :key #'attribute-name )))
    (when left-superscript
      (special-pattern  left-superscript))
    )
  )

(defmethod left-subscript-pattern ((math-object math-object))
  "Return  pattern for left-subscript if any "
  (let ((left-subscript (find 'left-subscript (attributes math-object )
                              :key #'attribute-name )))
    (when left-subscript
      (special-pattern  left-subscript))
    )
  )

;;}}}

;;{{{ classes for types from math classification:

;;; Class: ORDINARY                                          Author: raman
;;; Created: Thu Oct 29 12:49:48 1992

(defclass ordinary (math-object)
  ()
  (:documentation "Classified as ordinary in math mode"))

(defun make-ordinary ()
  (let ((self (make-instance 'ordinary)))
    self))


(defun ordinary-p (self)
  (typep self 'ordinary ))

  ;;; Class: FACTORIAL                                         Author: raman
  ;;; Created: Sun Feb  7 10:02:37 1993

(defclass factorial (ordinary)
  ()
  (:documentation "A factorial object"))

(defun make-factorial ()
  (let ((self (make-instance 'factorial)))
    self))

(defun factorial-p (self)
  (typep  self 'factorial))

;;; Class: UNARY-MINUS                                       Author: raman
;;; Created: Wed Nov  4 09:56:38 1992

(defclass unary-minus (math-object)
  ((n-args :initform 1  :initarg :n-args :accessor operator-n-args))
  (:documentation "Unary minus"))

(defun make-unary-minus ()
  (let ((self (make-instance 'unary-minus)))
    self))

(defun  unary-minus-p (self)
  (typep self 'unary-minus))

;;; Class: BINARY-OPERATOR                                   Author: raman
;;; Created: Thu Oct 29 12:50:38 1992

(defclass binary-operator (math-object)
  ((n-args :initform 2  :initarg :n-args :accessor operator-n-args))
  (:documentation "Binary operator "))

(defun make-binary-operator ()
  (let ((self (make-instance 'binary-operator)))
    self))


(defun binary-operator-p (self)
  (typep self  'binary-operator))

;;; Method: INCREMENT-N-ARGS                                 Author: raman
;;; Created: Sat Oct 31 14:25:55 1992

(defmethod increment-n-args ((binary-operator binary-operator))
  "Increment number of args"
  (incf (operator-n-args  binary-operator))
  )

;;; Class: RELATIONAL-OPERATOR                               Author: raman
;;; Created: Thu Oct 29 12:51:01 1992

(defclass relational-operator (math-object)
  ((n-args :initform 2  :initarg :n-args :accessor operator-n-args))
  (:documentation "Relational operator"))

(defun make-relational-operator ()
  (let ((self (make-instance 'relational-operator)))
    self))


(defun relational-operator-p (self)
  (typep self  'relational-operator))

;;; Method: INCREMENT-N-ARGS                                 Author: raman
;;; Created: Sat Oct 31 14:28:13 1992

(defmethod increment-n-args ((relational-operator relational-operator))
  "Increment number of args"
  (incf (operator-n-args  relational-operator))
  )
;;; Class: MATH-NUMBER                                       Author: raman
;;; Created: Fri Oct 30 09:28:46 1992

(defclass math-number (ordinary)
  ()
  (:documentation "A number in math"))

(defun make-math-number ()
  (let ((self (make-instance 'math-number)))
    self))

(defmethod math-number-p ((self math-number ))
  t)

(defmethod math-number-p ((object t)) nil)

;;; Class: ARROW-OPERATOR                                    Author: raman
;;; Created: Thu Oct 29 12:51:24 1992

(defclass arrow-operator (math-object)
  ((n-args :initform 2  :initarg :n-args :accessor operator-n-args))
  (:documentation "Arrow operator"))

(defun make-arrow-operator ()
  (let ((self (make-instance 'arrow-operator)))
    self))


(defun arrow-operator-p (self)
  (typep self  'arrow-operator))

;;; Method: INCREMENT-N-ARGS                                 Author: raman
;;; Created: Sat Oct 31 14:27:06 1992

(defmethod increment-n-args ((arrow-operator arrow-operator))
  "Increment number of args"
  (incf (operator-n-args  arrow-operator))
  )
;;; Class: BIG-OPERATOR                                      Author: raman
;;; Created: Thu Oct 29 12:51:47 1992

(defclass big-operator (math-object)
  ((n-args :initform 1  :initarg :n-args :accessor operator-n-args))
  (:documentation "Big operator"))

(defun make-big-operator ()
  (let ((self (make-instance 'big-operator)))
    self))


(defun big-operator-p (self)
  (typep  self 'big-operator))

;;; Method: INCREMENT-N-ARGS                                 Author: raman
;;; Created: Tue Nov  3 15:14:51 1992

(defmethod increment-n-args ((big-operator big-operator))
  "Increment number of args"
  (incf (operator-n-args big-operator))
  )

;;; Class: MATHEMATICAL-FUNCTION-NAME                        Author: raman
;;; Created: Thu Oct 29 12:52:22 1992

(defclass mathematical-function-name (math-object)
  ()
  (:documentation "Mathematical function name"))

(defun make-mathematical-function-name ()
  (let ((self (make-instance 'mathematical-function-name)))
    self))


(defun mathematical-function-name-p (self)
  (typep self 'mathematical-function-name))

(defun mathematical-function-name-subtype-p (self)
  (typep  self 'mathematical-function-name)
  )
;;; Class: QUANTIFIER                                        Author: raman
;;; Created: Thu Oct 29 13:21:58 1992

(defclass quantifier (math-object)
  ()
  (:documentation "Quantifier"))

(defun make-quantifier ()
  (let ((self (make-instance 'quantifier)))
    self))


(defun quantifier-p (self)
  (typep self 'quantifier))

;;; Class: NEGATION-OPERATOR                                 Author: raman
;;; Created: Thu Oct 29 13:22:14 1992

(defclass negation-operator (math-object)
  ((n-args :initform 1 :initarg :n-args :accessor operator-n-args))
  (:documentation "Negation operator"))

(defun make-negation-operator ()
  (let ((self (make-instance 'negation-operator)))
    self))


(defun negation-operator-p (self)
  (typep self  'negation-operator))

;;; Classes for the symbol classification for math mode:

;;}}}

;;{{{ accessors for relational-operators
;;; A relational operator that ha stwo children is a simple equation.
;;; One that has more than two children may be required to be
;;; interpreted as a set of equations, ie a=b=c is a = b and  b = c.
;;; The following method returns a list of equations defined by a
;;; relational

  ;;; Method: EQUATIONS                                        Author: raman
  ;;; Created: Tue Dec 15 13:42:45 1992

(defmethod equations ((relational-operator relational-operator))
  "Return list of equations defined by this relational"
  (let
      ((children (children  relational-operator ))
       (relational (contents relational-operator)))
    (if (= 2 (length children ))
        (list relational-operator)
        (butlast
         (loop for relation on children
               collect
               (make-instance  (type-of relational-operator)
                               :contents  relational
                               :children (list
                                          (first relation)
                                          (second relation ))))))
    )
  )

;;}}}

;;{{{ juxtaposition

(defclass juxtaposition (binary-operator) () )
(defun juxtaposition-p (self)
  (typep self 'juxtaposition ))
;;}}}
;;{{{ big-operator subclasses

;;; Generated by evaluating <(setup-operators-class-table)>
(DEFCLASS BIG-WEDGE (BIG-OPERATOR) NIL)
(DEFCLASS BIG-VEE (BIG-OPERATOR) NIL)
(DEFCLASS BIG-O-DOT (BIG-OPERATOR) NIL)
(DEFCLASS BIG-CAP (BIG-OPERATOR) NIL)
(DEFCLASS BIG-CUP (BIG-OPERATOR) NIL)
(DEFCLASS INTEGRAL (BIG-OPERATOR) NIL)
(DEFCLASS BIG-O-PLUS (BIG-OPERATOR) NIL)
(DEFCLASS SUMMATION (BIG-OPERATOR) NIL)
(DEFCLASS CO-PRODUCT (BIG-OPERATOR) NIL)
(DEFCLASS O-INTEGRAL (BIG-OPERATOR) NIL)
(DEFCLASS BIG-SQUARE-CUP (BIG-OPERATOR) NIL)
(DEFCLASS BIG-U-PLUS (BIG-OPERATOR) NIL)
(DEFCLASS PRODUCT (BIG-OPERATOR) NIL)
(DEFCLASS BIG-O-TIMES (BIG-OPERATOR) NIL)

;;; Function: CREATE-BIG-OPERATOR                            Author: raman
;;; Created: Wed Dec  9 13:37:54 1992

(defun create-big-operator (&key contents children)
  "Create big operator"
  (let
      ((name (get-operator-class-name contents )))
    (make-instance name
                   :contents contents
                   :children children)
    )
  )

;;}}}
;;{{{ integrals

;;; Class: INTEGRAL-D                                        Author: raman
;;; Created: Tue Nov  3 12:27:00 1992

(defclass integral-d (math-object)
  ()
  (:documentation "Integral close delimiter"))

(defun make-integral-d ()
  (let ((self (make-instance 'integral-d)))
    self))


(defun integral-d-p (self)
  (typep self  'integral-d))

;;; Function: INTEGRAL-D-SUBTYPE-P                           Author: raman
;;; Created: Tue Nov  3 13:00:47 1992

(defun integral-d-subtype-p (self)
  "Check if arg is an integral d or a subtype"
  (typep self 'integral-d)
  )

;;}}}
;;{{{ accessors for integral

  ;;; Method: INTEGRAND                                        Author: raman
  ;;; Created: Mon Dec 14 20:13:17 1992

(defmethod integrand ((integral integral))
  "Return integrand"
  (let ((children (children integral )))
    (cond
      ((= 2 (length  children))
       (first children))
      (t  children))
    )
  )

  ;;; Method: VARIABLE-OF-INTEGRATION                          Author: raman
  ;;; Created: Mon Dec 14 20:15:11 1992

(defmethod variable-of-integration ((integral integral))
  "Return variable of integration if specified. "
  (let ((children (children integral )))
    (when (and (listp children)
               (typep (second children ) 'integral-d))
      (second  children )))
  )

;;}}}

;;{{{ delimiter subclasses

;;; hand generated.
(defclass       ceiling-brackets (delimited-expression) () )
(defclass      floor-brackets  (delimited-expression) () )
(defclass                         braces (delimited-expression) () )
(defclass                        brackets (delimited-expression) () )
(defclass angle-brackets (delimited-expression) ())
(defclass       parenthesis (delimited-expression) () )

(defun ceiling-brackets-p (self)
  (typep self 'ceiling-brackets))

(defun floor-brackets-p (self)
  (typep self 'floor-brackets))

(defun braces-p (self)
  (typep self 'braces))

(defun brackets-p (self)
  (typep self 'brackets))

(defun angle-brackets-p (self)
  (typep self 'angle-brackets))

(defun parenthesis-p (self)
  (typep self 'parenthesis))

;;; Function: CREATE-DELIMITED-EXPRESSION                    Author: raman
;;; Created: Wed Dec  9 14:28:21 1992

(defun create-delimited-expression (&key contents children type
                                      open-delimiter close-delimiter)
  "Create a delimited expression. "
  (let ((name (get-operator-class-name contents )))
    (make-instance name
                   :contents contents
                   :children children
                   :type type
                   :open-delimiter  (if (stringp open-delimiter)
                                        open-delimiter
                                        (math-cs-name open-delimiter))
                   :close-delimiter (if (stringp close-delimiter )
                                        close-delimiter
                                        (math-cs-name close-delimiter )))
    ))

;;}}}

;;{{{ math subformula

;;; Class: MATH-SUBFORMULA                                   Author: raman
;;; Created: Sun Sep 27 18:39:51 1992

(defclass math-subformula (math-object)
  ((contents :initform nil :initarg :contents :accessor children
             :accessor contents))
  (:documentation "a subformula"))

(defun make-math-subformula ()
  (let ((self (make-instance 'math-subformula)))
    self))


(defun math-subformula-p (self)
  (typep self  'math-subformula))

;;}}}
;;{{{ root

;;; Class: GENERALIZED-ROOT                                  Author: raman
;;; Created: Tue Mar 16 12:19:31 1993

(defclass generalized-root (math-object)
  ()
  (:documentation "A generalized root object"))

(defun make-generalized-root ()
  (let ((self (make-instance 'generalized-root)))
    self))


(defun generalized-root-p (self)
  (eq (class-name (class-of self)) 'generalized-root))

(defun generalized-root-subtype-p (self)
  (typep  self 'generalized-root))

;;}}}
