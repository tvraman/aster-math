;;;   -*- Syntax: Common-Lisp;  Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :user)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))


;;; Wed Dec 23 12:07:24 EST 1992
;;; Simple hack to convert digits to cardinal numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;; Variable: *CARDINAL-NUMBERS-TABLE*                       Author: raman
  ;;; Created: Wed Dec 23 12:08:16 1992

(defvar *cardinal-numbers-table*
  (make-hash-table :test  #'equal)
  "Maps digit strings to cardinal numbers ")


  ;;; Function: DEFINE-CARDINAL-NUMBER                         Author: raman
  ;;; Created: Wed Dec 23 12:09:04 1992

(defun define-cardinal-number (string cardinal) 
  "Define cardinal number "
  (setf (gethash string *cardinal-numbers-table* ) cardinal)
  )


  ;;; Method: CARDINAL-NUMBER                                Author: raman
  ;;; Created: Wed Dec 23 12:09:45 1992

(defmethod  cardinal-number ((string  string))
  "Return cardinal number"
  (let  ((cardinal  (gethash string *cardinal-numbers-table* )))
    (or cardinal
        (concatenate 'string
                     string
                     "th" ))
    )
  )


  ;;; Method: CARDINAL-NUMBER                                  Author: raman
  ;;; Created: Wed Dec 23 12:23:11 1992

(defmethod cardinal-number ((n integer ))
  "Cardinal number for integers"
  (cardinal-number
   (format nil "~s" n ))
  )

(defmethod cardinal-number ((ordinary ordinary ))
  "Return cardinal number "
  (cardinal-number (contents  ordinary ))
  )


(defmethod cardinal-number ((math-subformula math-subformula ))
  (cardinal-number  (contents math-subformula ))
  )

  ;;; Method: CARDINAL-NUMBER                                  Author: raman
  ;;; Created: Fri Dec 25 17:08:46 1992

(defmethod cardinal-number ((math-object math-object))
  "Default method, just return math objects "
  math-object
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; definitions:
(define-cardinal-number "1" "first")
(define-cardinal-number "2" "second" )
(define-cardinal-number "3" "third" )
(define-cardinal-number "4" "fourth" )
(define-cardinal-number "5" "fifth" )
(define-cardinal-number "6" "sixth" )
(define-cardinal-number "7" "seventh" )
(define-cardinal-number "8" "eighth" )
(define-cardinal-number "9" "ninth" )
(define-cardinal-number "10" "tenth" )

;;; letters:

(define-cardinal-number "a"  "ayth")
(define-cardinal-number "b"  "beeth")
(define-cardinal-number "c"  "ceeth")
(define-cardinal-number "j" "jayth")
(define-cardinal-number "k" "kayth")
(define-cardinal-number "m" "emmeth")
(define-cardinal-number "n" "enneth")
(define-cardinal-number "p" "peeth")
