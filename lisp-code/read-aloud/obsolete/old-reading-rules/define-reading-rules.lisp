;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))
;;; Wed Nov 11 11:08:49 EST 1992
;;;
;;; Define a set of functions that allow easy switching of current
;;; read method for a particular object.
;;; Without this code, I currently need to compile in a particular
;;; read method for an object if I want to change how it is read.
;;; To simplify this, provide a define-reading-rule macro which takes
;;; a class name, a name for the rule, and a set of forms that would
;;; normally be the method body. Install this in a table, and provide
;;; a select-reading-rule macro which when called with a class name and
;;; rule name retrieves the body that was stored in the table for this
;;;  pair (class, rule-name) and compiles it as the current read
;;;  method for this class.


;;; Variable: *READING-RULE-TABLE*                           Author: raman
;;; Created: Wed Nov 11 11:12:49 1992

(defvar *reading-rule-table* nil "Table of reading rules")


;;; Class: READING-RULE                                      Author: raman
;;; Created: Wed Nov 11 11:13:37 1992

(defclass reading-rule ()
  ((rule-class :initform nil :initarg :rule-class :accessor rule-class)
   (rule-name :initform nil :initarg :rule-name :accessor rule-name)
   (rule-body :initform nil :initarg :rule-body :accessor rule-body))
  (:documentation "A reading rule "))

(defun make-reading-rule ()
  (let ((self (make-instance 'reading-rule)))
    self))

(proclaim '(inline reading-rule-p))

(defun reading-rule-p (self)
  (eq (class-name (class-of self)) 'reading-rule))


;;; Macro: DEFINE-READING-RULE                               Author: raman
;;; Created: Wed Nov 11 11:16:57 1992

(defmacro define-reading-rule ((&key rule-name class) &body body) 
  "Define a reading rule"
  `(let
    ((rule (make-instance 'reading-rule
                          :rule-class ',class
                          :rule-name ',rule-name
                          :rule-body  '(progn  ,@body ))))
    (delete-reading-rule ',rule-name ',class)
    (push rule *reading-rule-table*))
  )



;;; Function: DELETE-reading-RULE Author: raman
;;; Created: Sat Nov 14 12:24:34 1992

(defun delete-reading-rule (name class) 
  "Delete this rule"
  (setf *reading-rule-table*
        (remove-if #'(lambda(rule)
                       (and (equal (rule-name rule) name)
                            (equal (rule-class rule) class)))
                   *reading-rule-table*))
  )

;;; Function: GET-READING-RULE                               Author: raman
;;; Created: Wed Nov 11 11:35:36 1992

(defun get-reading-rule (name class) 
  "REtrieve reading rule"
  (let ((rule (find-if  #'(lambda(reading-rule)
                            (and (equal name (rule-name reading-rule))
                                 (equal class  (rule-class  reading-rule ))))
                        *reading-rule-table*)))
    (when rule (rule-body rule)))
  )


;;; Function: DEFINED-READING-RULE-P                         Author: raman
;;; Created: Wed Nov 11 15:26:04 1992

(defun defined-reading-rule-p (name class) 
  "Check if rule defined"
(let ((rule (find-if  #'(lambda(rule)
                            (and (equal name (rule-name rule) )
                                 (equal class  (rule-class  rule ))))
                        *reading-rule-table*)))
    (when rule t ))  
  )

;;; Macro: SELECT-READING-RULE                                  Author: raman
;;; Created: Wed Nov 11 11:25:11 1992

(defmacro select-reading-rule (&key name class ) 
  "Activate reading rule named name for class class"
  `(when ,(defined-reading-rule-p name class) 
    (defmethod read-aloud((,class ,class)) 
      , (get-reading-rule  name class )))
  )


;;; Function: PRODUCE-READ-ALOUD-METHOD Author: raman
;;; Created: Fri Dec  4 08:53:57 1992

(defun  produce-read-aloud-method  (rule) 
"produce defmethod from rule body " 
  `(defmethod read-aloud  (( ,(rule-class rule) ,(rule-class rule) ))
    ,(rule-body rule)
    )
  )


;;; Following macro fails to work when called from inside
;;; activate-reading-style. No error, in fact appears to be doing the
;;; right thing, but has no effect. Something to do with environment
;;; in which the defmethod gets defined.
;;; If this problem fixed, replace (generate-read-aloud-method rule)
;;; for  (eval (produce-read-aloud-method rule ))
;;; Thus avoiding the explicit call to eval. 
;;; Macro: GENERATE-READ-ALOUD-METHOD                        Author: raman
;;; Created: Sat Dec  5 16:44:45 1992

(defmacro generate-read-aloud-method (rule) 
  "Generate read aloud method "
  `(let
    ((class  (rule-class ,rule))
     (body (rule-body ,rule )))
    (defmethod read-aloud ((class class))
      body)
    )
  )
  
;;; Function: ACTIVATE-READING-STYLE                            Author: raman
;;; Created: Fri Dec  4 09:01:44 1992

(defun  activate-reading-style  (&key name) 
  "Activate all reading rules of this name"
  (let ((rules  (remove nil
                        (loop for rule in  *reading-rule-table*
                              collect (when
                                          (equal name (rule-name
                                                       rule))
                                        rule )))))
    (loop for rule in rules
          do
          (eval (produce-read-aloud-method rule ))
          )
    )
  )

