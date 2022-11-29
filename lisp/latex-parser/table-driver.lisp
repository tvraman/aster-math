;;;   -*-   Mode: LISP -*-    ;;;
 
 

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;


(in-package :aster)
 




;;; Mon Jan 27 09:38:08 EST 1992
 

;;; Contains the functions and variable definitions for making
;;; the parser table driven.
 

;;; Function: WHAT-IS?                                       Author: raman
;;; Created: Sun Jan 26 15:02:43 1992
;;; Modified: Mon Apr 20 17:36:27 EDT 1992
 

(defun what-is? (token) 
  "returns token marker for lists which is the first element, or word if string."
  (cond
    ((listp token) (first token))
    ((stringp token ) 'word)
    (t  (error "what-is? Do not know what ~a is" token))
    )
  )



;;; Function: MATH-WHAT-IS?                                  Author: raman
;;; Created: Tue Feb 25 13:13:22 1992


(defun math-what-is? (token) 
  "classify token according to math mode"
  (cond
    ((listp token) (first token))
    ((lookup-math-classification token ))
    ((number-string? token) 'number)
    ((stringp token)  'math-string)
    (t (error  	  "math-what-is? Cannot classify ~a"
                  token))
    )
  )



;;; Function: NUMBER-STRING?                            Author: raman
;;; Created: Thu Feb 27 10:52:18 1992

(defun NUMBER-STRING? (string) 
  "checks if string is a quoted number"
  (and
   (stringp string)
   (numberp
    (ignore-errors
     (read-from-string string ))))
  )

;;; Function: GET-PARSER                                     Author: raman
;;; Created: Sun Jan 26 15:06:13 1992
;;; Modified: Fri Dec 25 08:35:26 EST 1992
;;;  Switching to hash tables. <(list version backed up)>

(defun get-parser (token &key  (math-flag nil)) 
  "Get the right parsing function from the global variable "
  (let ((parser 
	(gethash
         (if math-flag   ; things classified differently in math mode.
             (math-what-is? token)
             (what-is? token))
                 *processing-function-table* )))
    (cond
      ((null parser )
       (gethash 'unknown-construct *processing-function-table*))
      (t parser ))))

 
;;; table for handling tex macros.
 


;;; Function: GET-TEX-MACRO                                  Author: raman
;;; Created: Thu Jan 30 09:35:39 1992
;;; Modified: Fri Dec 25 09:08:08 EST 1992
;;; <(Switching to hash tables, old version backed up. )>
(defun get-tex-macro (macro-name) 
  "gets the entry for macro macro-name from the table"
  (or
   (gethash  macro-name *tex-macro-table*)
   (gethash  'default *tex-macro-table* )))

;;; Macro: TOGGLE                                         Author: raman
;;; Created: Tue Feb  4 09:39:38 1992

(defmacro  toggle (global-var) 
  "toggle the setting of boolean variables."
  `(setf  ,global-var
    (not ,global-var))
  )
 
;;; end of file
 
