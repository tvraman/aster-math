
;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;

(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))



;;; This file sets up the mapping between class names and operator
;;; names.
;;; It is generated automatically by calling
;;; setup-operators-class-table defined in <(operator-classes.lisp)>
;;; Run the function defined in that file with a file name and then
;;; insert the contents of that file into this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variable: *OPERATOR-CLASS-NAMES*                         Author: raman
;;; Created: Wed Dec  9 11:44:16 1992

(defvar *operator-class-names*
  (make-hash-table :test #'equal)
  "Mapping between operators and their class names")

;;; Function: GET-OPERATOR-CLASS-NAME                                 Author: raman
;;; Created: Wed Dec  9 12:18:08 1992

(defun get-operator-class-name (operator) 
  "Return class name after evaling it from the table"
  (or (gethash operator *operator-class-names*)
      'math-object))
;;; big-operator ;;; 
(SETF (GETHASH "bigwedge" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-WEDGE)) 
(SETF (GETHASH "bigvee" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-VEE)) 
(SETF (GETHASH "bigodot" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-O-DOT)) 
(SETF (GETHASH "bigcap" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-CAP)) 
(SETF (GETHASH "bigcup" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-CUP)) 
(SETF (GETHASH "int" *OPERATOR-CLASS-NAMES*) (QUOTE INTEGRAL)) 
(SETF (GETHASH "bigoplus" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-O-PLUS)) 
(SETF (GETHASH "sum" *OPERATOR-CLASS-NAMES*) (QUOTE SUMMATION)) 
(SETF (GETHASH "coprod" *OPERATOR-CLASS-NAMES*) (QUOTE CO-PRODUCT)) 
(SETF (GETHASH "oint" *OPERATOR-CLASS-NAMES*) (QUOTE O-INTEGRAL)) 
(SETF (GETHASH "bigsqcup" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-SQUARE-CUP)) 
(SETF (GETHASH "biguplus" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-U-PLUS)) 
(SETF (GETHASH "prod" *OPERATOR-CLASS-NAMES*) (QUOTE PRODUCT)) 
(SETF (GETHASH "bigotimes" *OPERATOR-CLASS-NAMES*) (QUOTE BIG-O-TIMES)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delimiters ;;; hand generated
(setf (gethash "constraint-brackets" *operator-class-names* )
      'constraint-brackets)
(setf (gethash "ceiling-brackets" *operator-class-names* )  'ceiling-brackets)
(setf (gethash "floor-brackets" *operator-class-names* ) 'floor-brackets )
(setf (gethash                     "braces" *operator-class-names* )
      'braces )
(setf (gethash "brackets" *operator-class-names*) 'brackets) 
(setf (gethash                     "angle brackets"
                                   *operator-class-names* )  'angle-brackets)
(setf (gethash                     "paren" *operator-class-names* )
      'parenthesis  )
