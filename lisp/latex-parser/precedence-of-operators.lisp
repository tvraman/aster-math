;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)


;;; Wed Oct 28 10:51:15 EST 1992
;;; Implements precedence of operators.
;;; The precedence table is a list of lists.
;;; Each list  contains operators of the same precedence.
;;; The list of lists is ordered in order of increasing precedence.
;;; Provide operators for inserting an operator having the same
;;; precedence as an existing operator
;;; and for inserting a new row in the precedence table.
;;; Provide predicates  precedence-= precedence-> and precedence-<
;;; that the parser can use when testing precedence.


;;; Parameter: *PRECEDENCE-TABLE*                             Author: raman
;;; Created: Wed Oct 28 12:06:35 1992

(defparameter    *precedence-table*
  '(
    ("+" "-" addition)
    ("*" "/" multiplication)
    )
  "Precedence table")


;;; Function: PREC                                           Author: raman
;;; Created: Wed Oct 28 12:12:13 1992

(defun prec (operator) 
  "Return precedence of operator as an integer. For internal use:"
  (let
      ((precedence (loop for row in *precedence-table*
                         and
                         i = 0 then (+ i 1)
                         thereis
                         (when  (find operator row :test #'equal)
                           i))))
    (unless precedence
      (error "No precedence defined for ~a" operator))
    precedence)
  )


;;; Function: PRECEDENCE-DEFINED-P                           Author: raman
;;; Created: Wed Oct 28 16:03:15 1992

(defun precedence-defined-p (operator) 
  "Test if precedence already defined"
  (loop for row in *precedence-table*
        and
        i = 0 then (+ i 1)
        thereis
        (when  (find operator row :test #'equal)
          i))
  )

;;; Method: PRECEDENCE-=                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-= (operator-1 operator-2 &key (key #'identity))
  "Predicate for testing equality of precedence. "
  (=
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )

;;; Method: PRECEDENCE-<                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-< (operator-1  operator-2  &key (key #'identity))
  "Predicate tests if precedence less than" 
  (<
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )

;;; Method : PRECEDENCE->                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-> (operator-1 operator-2 &key (key #'identity))
  "Predicate: tests if precedence greater than"
  (>
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )

;;; Method: PRECEDENCE-=                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-= ((operator-1 math-object)
                          (operator-2 math-object)
                          &key (key #'contents))
  "Predicate for testing equality of precedence. "
  (=
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )

;;; Method: PRECEDENCE-<                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-< ((operator-1 math-object)  (operator-2 math-object)
                          &key (key #'contents))
  "Predicate tests if precedence less than"
  (<
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )

;;; Method : PRECEDENCE->                                   Author: raman
;;; Created: Wed Oct 28 12:28:48 1992

(defmethod  precedence-> ((operator-1 math-object)  (operator-2 math-object)
                          &key (key #'contents))
  "Predicate: tests if precedence greater than"
  (>
   (prec (funcall key operator-1))
   (prec (funcall key operator-2 )))
  )


;;; Method: PRECEDENCE->=                                    Author: raman
;;; Created: Thu Oct 29 16:12:08 1992

(defmethod precedence->= ((operator-1 math-object) (operator-2 math-object)
                          &key (key #'contents))
  "Predicate for testing precedence"
  (or
   (precedence-> operator-1 operator-2 :key key)
   (precedence-= operator-1 operator-2 :key key))
  )

;;; Method: PRECEDENCE-<=                                    Author: raman
;;; Created: Thu Oct 29 16:12:08 1992

(defmethod precedence-<= ((operator-1 math-object) (operator-2 math-object)
                          &key (key #'contents))
  "Predicate for testing precedence"
  (or
   (precedence-< operator-1 operator-2 :key key)
   (precedence-= operator-1 operator-2 :key key))
  )

;;; Function: DEFINE-PRECEDENCE                              Author: raman
;;; Created: Wed Oct 28 12:41:33 1992

(defun define-precedence (operator &key same-as) 
  "Define precedence for operator to be the same as an existing operator."
  (when  (prec same-as)
    (pushnew operator
             (elt  *precedence-table*  (prec same-as ))
             :test #'equal)
    )
  )


;;; Function: REMOVE-PRECEDENCE                              Author: raman
;;; Created: Wed Oct 28 15:37:32 1992

(defun remove-precedence (operator) 
  "Remove operator from precedence table"
  (when (prec operator)
    (setf      (elt *precedence-table*
                    (prec operator))
               (remove  operator
                        (elt *precedence-table*
                             (prec operator))
                        :test #'equal )))
  )


;;; Function: REDEFINE-PRECEDENCE                            Author: raman
;;; Created: Thu Oct 29 10:07:57 1992

(defun redefine-precedence (operator &key same-as ) 
  "Redefine precedence flushing previous precedence if any"
  (when (precedence-defined-p operator)
    (remove-precedence operator))
  (define-precedence operator :same-as same-as)
  )


;;; Function: ADD-PRECEDENCE-ROW                             Author: raman
;;; Created: Wed Oct 28 12:52:04 1992

(defun add-precedence-row (operator &key where   existing-operator)
  "Add a row to precedence table. "
  (assert (or ( eq where :before)
              (eq where :after)) nil
              "Where should be either :before or :after  not ~a"
              where)
  (unless (precedence-defined-p  operator) 
    (let
        ((pos (cond
                ((eq where :before )
                 (prec existing-operator)  )
                ((eq where :after)
                 (1+ (prec existing-operator)))
                (t (error "Illegal value for where: ~a" where))
                )))
  ;;; generate new table:
      (setf *precedence-table* (insert! (list operator)
                                        pos
                                        *precedence-table*))
      ))
  )


;;; Function: INSERT!                                        Author: raman
;;; Created: Wed Oct 28 15:16:04 1992

(defun insert! (item position original-list) 
  "Insert item at position position of original-list"
  (append
   (butlast original-list   (- (length original-list)
                               position )) ; front 
   (list item)                      ; insertion 
   (nthcdr position  original-list)     ; tail 
   )  
  )


;;; Function: REMOVE-PRECEDENCE-ROW                          Author: raman
;;; Created: Wed Oct 28 15:31:33 1992

(defun remove-precedence-row (operator) 
  "Remove precedence row containing this operator"
  (setf *precedence-table*
        (remove-if #'(lambda(row)
                       (find operator row) )
                   *precedence-table*))
  )

;;; Function: SETUP-PRECEDENCE-TABLE                         Author: raman
;;; Created: Wed Oct 28 15:56:29 1992


(defun setup-precedence-table () 
  "Setup precedence table of operators. "
  (add-precedence-row 'arrow-operator :where :before
                      :existing-operator  'addition)
  (add-precedence-row  'relational-operator :where  :before
                       :existing-operator 'arrow-operator)
  (add-precedence-row  'big-operator  :where :after
                       :existing-operator  'arrow-operator)
  (add-precedence-row  'logical-and    :where :before
                       :existing-operator 'addition)
  (add-precedence-row 'logical-or :where :before
                      :existing-operator 'logical-and)
  (add-precedence-row 'mathematical-function :where :after
                      :existing-operator 'multiplication)
  (add-precedence-row 'juxtaposition  :where :after
                      :existing-operator 'mathematical-function)
  (add-precedence-row 'unary-minus :where :after
                      :existing-operator 'juxtaposition)
  (add-precedence-row 'paran :where :after
                      :existing-operator 'unary-minus)
  (add-precedence-row 'quantifier :where :before
                      :existing-operator 'relational-operator)
  (add-precedence-row 'conditional-operator :where :before
                      :existing-operator 'quantifier)
  (add-precedence-row 'tex-infix-operator :where :before
                      :existing-operator 'conditional-operator)
  (add-precedence-row 'math-list-operator :where :before
                      :existing-operator 'conditional-operator)
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using (hash-key math-symbol) 
        when (equal 'arrow-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'arrow-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( hash-key math-symbol) 
        when (equal 'relational-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'relational-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using (hash-key math-symbol) 
        when (equal 'big-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'big-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( hash-key math-symbol) 
        when (equal 'mathematical-function-name  classification) 
        do (define-precedence 
               math-symbol :same-as 'mathematical-function))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( hash-key math-symbol) 
        when (equal 'quantifier  classification) 
        do (define-precedence 
               math-symbol :same-as 'quantifier))
  )

#+lucid
(defun setup-precedence-table () 
  "Setup precedence table of operators. "
  (add-precedence-row 'arrow-operator :where :before
                      :existing-operator  'addition)
  (add-precedence-row  'relational-operator :where  :before
                       :existing-operator 'arrow-operator)
  (add-precedence-row  'big-operator  :where :after
                       :existing-operator  'arrow-operator)
  (add-precedence-row  'logical-and    :where :before
                       :existing-operator 'addition)
  (add-precedence-row 'logical-or :where :before
                      :existing-operator 'logical-and)
  (add-precedence-row 'mathematical-function :where :after
                      :existing-operator 'multiplication)
  (add-precedence-row 'juxtaposition  :where :after
                      :existing-operator 'mathematical-function)
  (add-precedence-row 'unary-minus :where :after
                      :existing-operator 'juxtaposition)
  (add-precedence-row 'paran :where :after
                      :existing-operator 'unary-minus)
  (add-precedence-row 'quantifier :where :before
                      :existing-operator 'relational-operator)
  (add-precedence-row 'conditional-operator :where :before
                      :existing-operator 'quantifier)
  (add-precedence-row 'tex-infix-operator :where :before
                      :existing-operator 'conditional-operator)
  (add-precedence-row 'math-list-operator :where :before
                      :existing-operator 'conditional-operator)
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( key math-symbol) 
        when (equal 'arrow-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'arrow-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( key math-symbol) 
        when (equal 'relational-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'relational-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( key math-symbol) 
        when (equal 'big-operator classification) 
        do (define-precedence 
               math-symbol :same-as 'big-operator))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( key math-symbol) 
        when (equal 'mathematical-function-name  classification) 
        do (define-precedence 
               math-symbol :same-as 'mathematical-function))
  (loop for  classification being the hash-values of
        *math-classification-table* 
        using ( key math-symbol) 
        when (equal 'quantifier  classification) 
        do (define-precedence 
               math-symbol :same-as 'quantifier))
  )



;;; Function: SHOW-PRECEDENCE-TABLE                          Author: raman
;;; Created: Thu Oct 29 08:55:26 1992

(defun show-precedence-table () 
  "Show one entry from each row of precedence table"
  (loop for row in *precedence-table*
        and
        i = 0 then (+ i 1) 
        do
        (format t "~d: ~a ~%" i  (last row))
        )
  )
