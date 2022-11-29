;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;


(in-package :aster)

;;; Wed Dec  9 11:29:59 EST 1992
;;; Code to generate class definitions for operators.
;;; Should look in math classification table and generate class
;;; definitions for operators automatically.
;;; Set up a mapping between operator names and the names to be used
;;; for the classes.
;;; This is because the operators currently have the same names as
;;; used by TeX and hence not nemonic.
;;; eg: int should have its class called integral etc.
;;; Generate this mapping from the pronunciation table.
 





;;; Variable: *OPERATORS-IN-CLASSIFICATION*                  Author: raman
;;; Created: Wed Dec  9 11:46:00 1992

(defvar *operators-in-classification*
  '(big-operator )
  "List of classifications that are operators. ")

;;; Variable: *OPERATOR-CLASS-NAMES*                         Author: raman
;;; Created: Wed Dec  9 11:44:16 1992

(defvar *operator-class-names*
  (make-hash-table :test #'equal)
  "Mapping between operators and their class names")


;;; Function: SETUP-OPERATORS-CLASS-TABLE                    Author: raman
;;; Created: Wed Dec  9 11:46:55 1992

(defun setup-operators-class-table (operator-type file-name) 
  "Setup mapping between operators and their class names, prompting user
for the class name"
  (with-open-file  (stream file-name :direction :output) 
    (loop for key  being the hash-keys of *math-classification-table*
          using (value   symbol-type)
          when (equal symbol-type operator-type)
          do
          (format t "Enter class name for operator ~a ~%" key)
          (format stream  "~s ~%"
                  `(setf (gethash ,key *operator-class-names* )
                    ,(read)))
          ))
  )



;;; Function: CREATE-OPERATOR-CLASSES                        Author: raman
;;; Created: Wed Dec  9 11:40:51 1992

(defun create-operator-classes (super-type) 
  "Create operator class definitions "
  (loop for    operator being the hash-keys of
        *math-classification-table*
        using (value op-type) 
        when (equal super-type op-type) 
        do 
        (format t "~s ~%" 
                `(defclass ,(get-operator-class-name operator)  (,super-type) () )
                )
        )
  )

