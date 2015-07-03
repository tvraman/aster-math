;;;   -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-    ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (C) 1990, 1991, 1992, 1993, 1994by T. V. Raman 
;;; All Rights Reserved
;;;
(in-package :parser)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

;;; Thu Apr  8 09:30:29 EDT 1993
;;; Children and attributes for standard math objects have specific
;;; names.
;;; Enter them here:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Organize this file as follows:
;;; A fold for each object
;;;
;;; {summation

(call-its-children summation (summand))
(call-its-attributes summation ((subscript "lower constraint ")
                                (superscript "upper constraint ")))

;;; }
;;; {Integral

(call-its-children integral ( " integrant " ))
(call-its-attributes integral ((subscript lower-limit)
                               (superscript upper-limit)))

;;; }
;;; {Juxtaposition

(call-its-children juxtaposition term) 



;;; }
;;; {Parenthesis

(call-its-children parenthesis ( " parenthesized expression " ))

;;; }
;;; {relational operator

(call-its-children relational-operator (" left hand side "  " right hand side "))
;;; }
;;; {arrow operator

(call-its-children arrow-operator (" left hand side " " right hand side " ))

;;; }

