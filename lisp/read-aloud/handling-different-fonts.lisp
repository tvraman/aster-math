;;;   -*-   Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :aster)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :aster)

;;; Contains code for handling different fonts when reading alod.
;;; Implement a hash table which contains font names and a function to
;;; set afl state.
;;; Read aloud methods can funcall this when setting local state.


;;; Variable: *FONT-AFL-STATE-TABLE*                             Author: raman
;;; Created: Thu Sep  3 09:45:41 1992

(defvar *font-afl-state-table*
  (make-hash-table :test #'equal   )
  "Hash table holding mapping from fonts to afl states")


;;; This function assumes fonts stored as lists whose first element is
;;; 'font and the second element the font name

;;; Function: FONT-NAME                                      Author: raman
;;; Created: Thu Sep  3 09:48:56 1992

(defun font-name (font) 
  "return font name"
  (assert (valid-font? font)  nil
          "~a is not a font"
          font)
  (second font)
  )


;;; Function: VALID-FONT?                                    Author: raman
;;; Created: Thu Sep  3 10:04:13 1992

(defun valid-font? (font) 
  "validate font"
  (and
   (listp font)
   (equal (first font) 'font))
  )


;;; Function: DEFINE-FONT-RULE                               Author: raman
;;; Created: Thu Sep  3 09:57:56 1992

(defun define-font-rule (font-name afl-setter) 
  "define afl-setter as the action to be taken for handling font font-name"
  (setf (gethash font-name *font-afl-state-table* )
        afl-setter)
  )

;;; default is identity 
;;; Function: GET-FONT-RULE                                  Author: raman
;;; Created: Thu Sep  3 09:59:31 1992

(defun get-font-rule (font-name) 
  "Retrive afl setter for this font"
  (or 
   (gethash font-name *font-afl-state-table*)
   #'identity)
  )




;;; Function: RETRIEVE-FONT-RULE                             Author: raman
;;; Created: Thu Sep  3 10:06:51 1992

(defun retrieve-font-rule (font) 
  "retrieve font rule for this font"
  (get-font-rule (font-name font))
  )

